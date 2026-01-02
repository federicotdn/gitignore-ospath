# gitignore-ospath

A compact Haskell library for reading `.gitignore` files and filtering paths.

The library should also work with other `.gitignore`-like formats, e.g. `.dockerignore`. The rules for parsing and filtering where taken from the [Git documentation pages](https://git-scm.com/docs/gitignore). Input paths must be `OsPath`.

> [!WARNING]
> This library is still at a very early development phase, so it may be missing some features. The API may also be changed significantly.

## Usage

Given the following `.gitignore` file:

```gitignore
# Lines starting with '#' are ignored.
# Empty lines are ignored as well.

/foo
/bar/

/baz/**
!/baz/special

*.py
```

The library can be used as follows:

```haskell
> import Ignore (parse, ignores)
> import qualified Data.Text.IO as TIO
> import System.OsPath (unsafeEncodeUtf)

> :t parse
parse :: Text -> Ignore

> :t ignores
ignores :: Ignore -> OsPath -> Bool -> Bool

> os = unsafeEncodeUtf
> text <- TIO.readFile ".gitignore"
> ign = parse text

> ignores ign (os "foo") False
True
> ignores ign (os "bar") True
True
> ignores ign (os "baz/a/b/c") False
True
> ignores ign (os "baz/special") False
False
> ignores ign (os "foo.py") False
True
```

The `ignores'` variant of `ignores` takes an `OsPath` that was already split (i.e. `[OsPath]`) instead of `OsPath` directly. This is useful for contexts where the user is building paths by segment, for example when recursing into a directory. Using `ignores'` in this case help avoid making calls to `splitDirectories`.

## Caveats

Patterns containing ranges (`[a-zA-z]`), optional characters (`a?`), wildcards (`*`) and escape sequences (`\*`) are not yet supported, and will be interpreted literally.

As an exception, patterns in the form of `*abc` or `abc*` are supported, which should cover a good amount of patterns such as `.*` (hidden files) or `*.py` (file extensions). Plain `*` is supported as well.
