-- | Gitignore pattern matching for 'OsPath'.
module Ignore
  ( Ignore (..),
    parse,
    ignores,
    ignores',
  )
where

import Ignore.Internal
