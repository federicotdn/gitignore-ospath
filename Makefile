.PHONY: build lint test repl install fmt docs

build:
	cabal build -j

lint:
	hlint src test

test:
	cabal test -j --enable-tests

repl:
	cabal repl

fmt:
	@ormolu -m inplace $$(git ls-files '*.hs')

docs:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump
