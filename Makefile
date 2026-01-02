.PHONY: build lint test repl install fmt

build:
	cabal build -j

lint:
	hlint src test

test:
	cabal test -j

repl:
	cabal repl

fmt:
	@ormolu -m inplace $$(git ls-files '*.hs')
