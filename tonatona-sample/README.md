# tonatona-sample

A example app using `tonatona` and its plugins.

```
$ ENV="Production" TONA_DB_DB_STRING="foo" make run
...
stack exec -- tonatona-sample-app
Generationg dummy Connection Pool...
"foo"
This function can use shared connection pool: Dummy Connection Pool
This function can use ENV environment variable to decide behaviour: Production
Migrating: foo
dbPool (Dummy Connection Pool) is shared
This function can use shared connection pool: Dummy Connection Pool
This function can use ENV environment variable to decide behaviour: Production
Migrating: foo
```
