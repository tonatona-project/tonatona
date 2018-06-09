.PHONY: build clean dump-th ghci haddock haddock-server hlint lint repl run run-fast test watch watch-haddock watch-tests watch-test
all: build

build:
	stack build tonatona-sample

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

# Run the server.
run: build
	stack exec -- tonatona-sample-app

# Build the executables quickly and then run the server.
run-fast: build-fast
	stack exec -- tonatona-sample-app

test:
	stack test

# Watch for changes.
watch:
	stack build --file-watch --fast tonatona-sample

# Watch for changes while trying to build haddocks
watch-haddock:
	stack build --haddock --file-watch --fast tonatona-sample

# Watch for changes.
watch-test: watch-tests
watch-tests:
	stack test --file-watch --fast tonatona-sample
