-- | Gitignore pattern matching for 'System.OsPath.OsPath'.
module Ignore
  ( Ignore (..),
    parse,
    ignores,
    ignores',
  )
where

import Ignore.Internal
