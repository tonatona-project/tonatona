.PHONY: build clean dump-th ghci haddock haddock-server hlint lint repl test watch watch-haddock watch-tests watch-test
all: build

build:
	stack build tonatona-db-sqlite

clean:
	stack clean

# dump the template haskell
# (you must have a compile error in the code you want to dump)
dump-th:
	-stack build --ghc-options="-ddump-splices"
	@echo
	@echo "Splice files:"
	@echo
	@find "$$(stack path --dist-dir)" -name "*.dump-splices"

ghci:
	stack ghci

heroku-release:
	heroku container:push web

haddock:
	stack build --haddock

# This runs a small python websever on port 8001 serving up haddocks for
# packages you have installed.
#
# In order to run this, you need to have run `make build-haddock`.
haddock-server:
	cd "$$(stack path --local-doc-root)" && python -m http.server 8001

hlint: lint

lint:
	hlint src/

# Start a repl.
repl: ghci

test:
	stack test

# Watch for changes.
watch:
	stack build --file-watch --fast tonatona-db-sqlite

# Watch for changes while trying to build haddocks
watch-haddock:
	stack build --haddock --file-watch --fast tonatona-db-sqlite

# Watch for changes.
watch-test: watch-tests
watch-tests:
	stack test --file-watch --fast tonatona-db-sqlite
