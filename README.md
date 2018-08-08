# Tonatona

![tonatona](https://user-images.githubusercontent.com/1481749/38623497-c455af60-3de0-11e8-8683-a215d074e7e0.jpg)

## What is Tonatona?

Tonatona is a **meta** application framework. It handles lots of boring tasks
that are needed for real-world development such as reading in values
defined in environment variables, setting up logging, sending emails, accessing
databases, etc.

Tonatona can also be used with your favorite web framework as a meta web
application framework.  Tonatona **does not** provide the core functionalities
of web applications, such as routing, request parsing, response building, etc.
Instead, you can use plugins like `tonatona-servant`, `tonatona-spock`, or
`tonatona-yesod` to work with your favorite web framework.

Tonatona provides a plugin architecture so that anyone can add plugins
implementing arbitrary functionality.  This repository contains many standard
plugins that are helpful when writing Haskell applications.

## Goals for Tonatona

The most important goal of Tonatona is to make development speed fast and
maintenance cost low.

In the Haskell community, you often hear things like, "Haskell makes it easy to
maintain applications, but it takes a lot of time to create completely new,
production-ready applications."

Tonatona's goal is to change this to "Haskell is great to maintain big
applications, AND it is super-easy to create completely new, production-ready
applications!"

Tonatona achieves this goal by providing a plugin-based architecture.  There
are many production-ready plugins to use in your own code.  In order to start
using a new plugin, often all you have to do is add a few additional
configuration options.

## How to use Tonatona

(TODO)

## Available Plugins

(TODO)

## Additional Features

Tonatona has the additional general features that apply to every plugin:

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

In general, new plugins will be accepted to Tonatona if they are widely useful.
For instance, a plugin adding support for a widely used database library will
probably be accepted, while a plugin adding support for a proprietary library
not widely used will probably not be accepted.

If your plugin is not accepted into this repository, you are free to support it
as a third-party repository, release it on Hackage, etc.  If you are using
Tonatona in a larger project, you will probably end up creating a few of your
own plugins!

## Maintainers

- [Kadzuya OKAMOTO](https://github.com/arowM)
- [Dennis Gosnell](https://github.com/cdepillabout)
