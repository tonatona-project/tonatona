# Tonatona

![tonatona](https://user-images.githubusercontent.com/1481749/38623497-c455af60-3de0-11e8-8683-a215d074e7e0.jpg)

## What is Tonatona?

Tonatona is **meta** web application framework. It handles lots of boring tasks
that are needed for real-world web development such as reading in values
defined in environment variables, setting up logging, sending emails, accessing
databases, etc.

Tonatona **does not** provide the core functionalities of web applications,
such as routing, request parsing, response building, etc.  Instead, you can
use Tonatona with your favorite web framework.  Tonatona should work with Servant,
Spock, Yesod, and others.

Tonatona provides a plugin architecture so that anyone can add plugins
implementing arbitrary functionality.  This repository contains many standard
plugins that are helpful when writing Haskell applications.

## How to use Tonatona

(TODO)

## Available Plugins

(TODO)

## Goals for Tonatona

Tonatona is meant to make it easy to write web applications.  Its goal is to
free the end-user from having to write anything but their application's
business logic.

We have the following goals and objectives in mind when developing Tonatona and
the plugins.

-   Tonatona should make it easy to get up and running with writing web
    applications.  It is perfect when you are on a small team.

    When you team or project grows larger, it may be worthwhile to move away
    from Tonatona onto hand-written logging, DB access, etc.

-   Make the end-user write a little boilerplate code up front in order to provide
    ease-of-use when writing their own business logic.

-   At runtime, plugins should be able to be configured by providing
    environment variables.  Changing the value of the environment variables
    should change the runtime behavior of your code.

-   It should be possible to switch plugins just by rewriting the import statements.

    For instance, it should be possible to go from using `tonatona-db-sqlite` to
    `tonatona-db-postgresql` just by changing the import statement

    ```haskell
    import qualified Tonatona.Db.Sqlite as TonaDb
    ```

    to

    ```haskell
    import qualified Tonatona.Db.Postgresql as TonaDb
    ```

## Contributing

Information about contributing new plugins can be found in
[CONTRIBUTING.md](./CONTRIBUTING.md).

## Maintainers

- [Kadzuya OKAMOTO](https://github.com/arowM)
- [Dennis Gosnell](https://github.com/cdepillabout)
