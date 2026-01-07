module Ignore.Internal where

import Data.List (tails)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Encoding (utf16le, utf8)
import System.OsPath (OsPath, splitDirectories)
import System.OsString (OsChar, encodeWith)
import qualified System.OsString as OS
import Text.ParserCombinators.ReadP (ReadP)
import qualified Text.ParserCombinators.ReadP as R

data Segment
  = DAsterisk
  | Asterisk
  | Const OsPath
  | Prefix OsPath
  | Suffix OsPath
  | Glob [GlobPart]
  deriving (Show, Eq)

data GlobClassPart = ClassSingle OsChar | ClassRange (OsChar, OsChar) | ClassSep deriving (Show, Eq)

data GlobPart = Wildcard Bool | Single OsChar | Class [GlobClassPart] | Noop deriving (Show, Eq)

data Pattern = Pattern
  { pSegments :: [Segment],
    pDir :: Bool,
    pNegated :: Bool,
    pAnchored :: Bool
  }
  deriving (Show, Eq)

-- | The parsed contents of a gitignore file.
--
-- Multiple 'Ignore' values can be combined using '<>' to merge their patterns.
-- This is useful for combining patterns from multiple gitignore files (e.g.,
-- a global gitignore and a repository-specific one). Later patterns take
-- precedence when determining if a path should be ignored.
newtype Ignore = Ignore [Pattern] deriving (Show, Eq)

-- | Combine gitignore files by appending their patterns.
--
-- When combining @ignore1 <> ignore2@, patterns from @ignore2@ are appended
-- after @ignore1@, meaning later patterns take precedence for matching.
instance Semigroup Ignore where
  (Ignore p1) <> (Ignore p2) = Ignore (p1 <> p2)

encodeChar :: Char -> Maybe OsChar
encodeChar ch =
  let str = [ch]
      encoded = OS.unpack <$> encodeWith utf8 utf16le str
   in case encoded of
        Left _ -> Nothing
        Right [] -> Nothing
        Right (osch : _) -> Just osch

getOsChar :: ReadP (Char, OsChar)
getOsChar = do
  ch <- R.get
  let osch = encodeChar ch
  case osch of
    Just osch' -> pure (ch, osch')
    Nothing -> R.pfail

finishRange :: (OsChar, OsChar) -> [GlobClassPart]
finishRange (start, end) =
  if start < end
    then [ClassRange (start, end), ClassSep]
    else [ClassSingle start, ClassSep]

parseGlobInner :: [GlobPart] -> Maybe [GlobClassPart] -> ReadP [GlobPart]
parseGlobInner curr class_ = do
  atEnd <- null <$> R.look
  case class_ of
    -- We are outside a class definition e.g. ab|cd[efg]
    Nothing ->
      if atEnd
        then return curr -- Finished!
        else do
          (ch, osch) <- getOsChar
          case ch of
            '[' -> contWith Noop (Just []) -- Start building a class.
            '?' -> contWith (Wildcard False) Nothing
            '*' -> contWith (Wildcard True) Nothing
            '\\' -> do
              -- Maybe read one more char.
              (_, osch') <- getOsChar
              contWith (Single osch') Nothing
            _ -> contWith (Single osch) Nothing -- Includes ']' case.

    -- We are inside a class definition e.g. abcd[e|fg]
    Just class_' -> do
      -- The line below will fail if there's nothing to read, i.e.
      -- the class was opened but never closed.
      (ch, osch) <- getOsChar
      case ch of
        ']' -> do
          let classParts = filter (/= ClassSep) class_'
          contWith (if null classParts then Noop else Class classParts) Nothing
        '-' -> do
          case class_' of
            [] -> contWith Noop $ Just [ClassSingle osch]
            elems -> case last elems of
              ClassRange (start, _) -> contWith Noop $ Just (init class_' ++ finishRange (start, osch))
              -- Start building a range.
              ClassSingle cs -> contWith Noop $ Just (init class_' ++ [ClassRange (cs, cs)])
              ClassSep -> contWith Noop $ Just (class_' ++ [ClassSingle osch]) -- Just add '-'.
        _ -> do
          -- Maybe read one more char.
          (_, osch') <- if ch == '\\' then getOsChar else pure (ch, osch)
          case class_' of
            [] -> contWith Noop $ Just [ClassSingle osch']
            elems -> case last elems of
              ClassRange (start, _) -> contWith Noop $ Just (init class_' ++ finishRange (start, osch'))
              _ -> contWith Noop $ Just (class_' ++ [ClassSingle osch'])
  where
    contWith p = parseGlobInner (curr ++ [p])

parseGlob :: Text -> Maybe Segment
parseGlob source = do
  let result = R.readP_to_S (parseGlobInner [] Nothing) (T.unpack source)
  case result of
    [] -> Nothing
    (r : _) -> do
      let parts = filter (/= Noop) $ fst r
      case parts of
        [] -> Nothing
        _ -> Just (Glob parts)

parseSegment :: Text -> Maybe Segment
parseSegment source
  | source == "**" = Just DAsterisk
  | source == "*" = Just Asterisk
  | scount == 0 && acount == 1 && aprefix = Suffix . OS.tail <$> encoded
  | scount == 0 && acount == 1 && asuffix = Prefix . OS.init <$> encoded
  | scount > 0 || acount > 0 = parseGlob source
  | otherwise = Const <$> encoded
  where
    aprefix = "*" `T.isPrefixOf` source
    asuffix = "*" `T.isSuffixOf` source
    acount = T.count "*" source
    scount = T.length (T.filter (`elem` ['[', ']', '?', '\\']) source)
    encoded = either (const Nothing) Just $ encodeWith utf8 utf16le (T.unpack source)

parsePattern :: Text -> Maybe Pattern
parsePattern source =
  let (source', negated) =
        if not (T.null source) && T.head source == '!'
          then (T.tail source, True)
          else (source, False)
      segments = filter (not . T.null) $ T.splitOn "/" source'
      segments' = mapMaybe parseSegment segments
      dir = "/" `T.isSuffixOf` source'
      anchored = "/" `T.isPrefixOf` source' || length segments' > 1
   in if null segments'
        then Nothing
        else
          Just
            Pattern
              { pSegments = segments',
                pDir = dir,
                pNegated = negated,
                pAnchored = anchored
              }

-- | Parse a complete gitignore file content into an 'Ignore' collection.
--
-- Strips whitespace, removes comments (lines starting with @#@), and parses
-- each remaining line as a pattern.
parse :: Text -> Ignore
parse source =
  let sourceLines = filter (not . T.null) $ map T.strip $ T.lines source
      patterns = filter (\l -> T.head l /= '#') sourceLines
   in Ignore (mapMaybe parsePattern patterns)

segmentMatches :: Segment -> OsPath -> Bool
segmentMatches target path = case target of
  Asterisk -> True
  Const val -> path == val
  Suffix val -> val `OS.isSuffixOf` path
  Prefix val -> val `OS.isPrefixOf` path
  Glob _ -> error "Unhandled Glob"
  -- NOTE: DAsterisk is already handled in 'patternIgnoresInner'.
  DAsterisk -> error "Unhandled ** pattern"

patternIgnoresInner :: Pattern -> [OsPath] -> Maybe Bool
patternIgnoresInner pat@Pattern {pNegated = True} splitPath =
  not <$> patternIgnoresInner pat {pNegated = False} splitPath
patternIgnoresInner pat@Pattern {pSegments = segs} splitPath =
  case splitPath of
    [] ->
      -- Exhausted path without returning False, meaning we might
      -- have an ignore-match. This depends on whether there is more
      -- pattern to match.
      if null segs then Just True else Nothing
    (pathHead : pathTail) -> case segs of
      [] -> Nothing
      (DAsterisk : rest) ->
        if null rest
          then Just True
          else
            let matches = mapMaybe (patternIgnoresInner (pat {pSegments = rest})) (init $ tails splitPath)
             in if null matches then Nothing else Just (or matches)
      (seg : rest) ->
        let match = segmentMatches seg pathHead
            continued = patternIgnoresInner (pat {pSegments = rest}) pathTail
            retry = if null pathTail then Nothing else patternIgnoresInner pat pathTail
         in if pAnchored pat
              then if match then continued else Nothing
              else (if match && isJust continued then continued else retry)

patternIgnores :: Pattern -> [OsPath] -> Bool -> Maybe Bool
patternIgnores pat splitPath dir =
  if (pDir pat && not dir) || null splitPath
    then Nothing
    else patternIgnoresInner pat splitPath

ignoresInner :: Ignore -> [OsPath] -> Bool -> Bool
ignoresInner (Ignore patterns) splitPath dir =
  last $ False : mapMaybe (\pat -> patternIgnores pat splitPath dir) patterns

-- | Check if an 'Ignore' collection ignores the given path.
--
-- The third parameter indicates whether the path is a directory.
ignores :: Ignore -> OsPath -> Bool -> Bool
ignores ign path = ignoresInner ign (splitDirectories path)

-- | Check if an 'Ignore' collection ignores the given pre-split path.
--
-- Like 'ignores', but takes a path already split into components. The third
-- parameter indicates whether the path is a directory.
ignores' :: Ignore -> [OsPath] -> Bool -> Bool
ignores' = ignoresInner
