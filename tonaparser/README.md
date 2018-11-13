# tonaparser

Integrated parser library created for tonatona meta application framework.

It can construct system configuration from environment variables, command line arguments, and any IO values depends on them.
See details for `example/Main.hs`.

## Build example app

```
$ stack build --pedantic --flag tonaparser:buildexample tonaparser
```

Then check how it works with various command line options and environment variables.

```
$ stack exec tonaparser-example -- --new-foo 3 --bar-baz foo
Foo {foo = 3, bar = Bar {baz = "foo"}}

$ stack exec tonaparser-example -- --new-foo 3
Foo {foo = 3, bar = Bar {baz = "baz"}}

$ stack exec tonaparser-example -- --foo 3 --bar-baz foo
tonaparser-example: No required configuration for "Configuration for Foo.foo"

$ FOO=5 stack exec tonaparser-example -- --bar-baz foo
Foo {foo = 5, bar = Bar {baz = "foo"}}
```
