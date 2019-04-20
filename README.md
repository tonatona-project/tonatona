# Tonatona

[![Build Status](https://secure.travis-ci.org/tonatona-project/tonatona.svg)](http://travis-ci.org/tonatona-project/tonatona)
[![Hackage](https://img.shields.io/hackage/v/tonatona.svg)](https://hackage.haskell.org/package/tonatona)
[![Stackage LTS](http://stackage.org/package/tonatona/badge/lts)](http://stackage.org/lts/package/tonatona)
[![Stackage Nightly](http://stackage.org/package/tonatona/badge/nightly)](http://stackage.org/nightly/package/tonatona)

![tonatona](https://user-images.githubusercontent.com/1481749/38623497-c455af60-3de0-11e8-8683-a215d074e7e0.jpg)

Any PRs are welcome, even for documentation fixes. (The main author of this library is not an English native.)

## What is Tonatona?

Tonatona is a framework for any type of applications. It handles lots of boring tasks
that are needed for real-world development such as reading in values
defined in environment variables, setting up logging, sending emails, accessing
databases, etc.

Tonatona can also be used with your favorite web framework as a glue.  Tonatona **does not** provide the core functionalities
of web applicationsわかってないかもしれません）, such as routing, request parsing, response building, etc.
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
using a new plugin, often all you have to do is just import it!  No need to
specify configuration from within your application.

## How to use Tonatona

Using Tonatona is relatively simple. It requires declaring a few datatypes, as
well as instances for classes provided by Tonatona.

This section describes how to do this, using our stack template for tonatona.

### A quick sample

First, let's create a new tonatona project with `stack new` command:

```bash
$ stack new sample-app https://raw.githubusercontent.com/tonatona-project/tonatona/master/tonatona.hsfiles
```

This will create a new project named "sample-app".

Let’s start by just looking at all the code in `sample-app/src/TonaApp/Main.hs`.

```haskell
module TonaApp.Main (app) where

import Tonalude

import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger



-- App


app :: RIO Config ()
app = do
  -- Tonatona.Logger plugin enables to use logger functions without any configurations.
  TonaLogger.logInfo $ display ("This is a skeleton for tonatona project" :: Text)
  TonaLogger.logDebug $ display ("This is a debug message" :: Text)



-- Config


data Config = Config
  { tonaLogger :: TonaLogger.Config
  -- , anotherPlugin :: TonaAnotherPlugin.Config
  -- , yetAnotherPlugin :: TonaYetAnotherPlugin.Config
  }


instance HasConfig Config TonaLogger.Config where
  config = tonaLogger


instance HasParser Config where
  parser = Config
      <$> parser
      -- <*> parser
      -- <*> parser
```

As you can see `import` part, tonatona is supposed to be used with `Tonalude` as an alternative to Prelude.

```haskell ignore
import Tonalude

import Tonatona (HasConfig(..), HasParser(..))
import qualified Tonatona.Logger as TonaLogger
```

The main function named `app` has type of `RIO Config ()`, instead of just `IO ()`.
The `Tonalude` module and plugin modules (e.g., Tonatona.Logger) provides
bunch of convenient functions to be used in `RIO Config` monad.

```haskell ignore
app :: RIO Config ()
app = do
  -- Tonatona.Logger plugin enables to use logger functions without any configurations.
  TonaLogger.logInfo $ display ("This is a skeleton for tonatona project" :: Text)
  TonaLogger.logDebug $ display ("This is a debug message" :: Text)
```

One of the amazing thing here is that there are no configurations about logging behaviour.
The only thing you have to do is just write a little bit of boilerplate code.

```haskell ignore
data Config = Config
  { tonaLogger :: TonaLogger.Config
  -- , anotherPlugin :: TonaAnotherPlugin.Config
  -- , yetAnotherPlugin :: TonaYetAnotherPlugin.Config
  }


instance HasConfig Config TonaLogger.Config where
  config = tonaLogger


instance HasParser Config where
  parser = Config
      <$> parser
      -- <*> parser
      -- <*> parser
```

As comment implies, there are no dificulties to use other plugins.
Just add boilerplate codes. It's all!

OK. It's time to compile it.

```bash
$ stack install --pedantic
```

So, let's see how it works.

```bash
$ stack exec sample-app
2018-11-18 21:15:09.594168: [info] This is a skeleton for tonatona project
@(src/TonaApp/Main.hs:16:3)
2018-11-18 21:15:09.594783: [debug] This is a debug message
@(src/TonaApp/Main.hs:17:3)
```

Wow, It actually works!
But wait, it seems too verbose to run on production servers.
Let's tell "sample-app" to act as production mode.

```bash
$ ENV=Production stack exec sample-app
This is a skeleton for tonatona project
```

Of course, all available environment variables and command line options can be displayed:

```bash
$ stack exec sample-app -- --help
Application deployment mode to run
    Default: Development
    Type: DeployMode
    Command line option: --env
    Environment variable: ENV

Make the operation more talkative
    Default: False
    Type: Bool
    Command line option: --verbose
    Environment variable: VERBOSE
...
...
```

This amazing feature is also provided by `tonatona-logger` plugin.
It is the power of plugin-based architecture tonatona provides.

### Adding new plugin

First, we need to add new plugin to use in `dependencies` of `package.yaml`.
In this example, we use `tonatona-persistent-sqlite` plugin.

```yaml
dependencies:
  - base >= 4.7 && < 5
  # `persistent` and `persistent-template` are also needed to
  # actually use `tonatona-persistent-sqlite`.
  - persistent
  - persistent-template
  - tonalude
  - tonatona
  - tonatona-logger
  # new plugin to add
  - tonatona-persistent-sqlite
```

Next, you need to add new field to `Config`.

```haskell ignore
import qualified Tonatona.Persist.Sqlite as TonaDb

data Config = Config
  { tonaLogger :: TonaLogger.Config
  , tonaDb :: TonaDb.Config
  }
```

Note that you have to import `Tonatona.Persist.Sqlite` module that `tonatona-persistent-sqlite` exposes.

Your `Config` data type will contain configuration values that can be read in on
the command line or through environment variables.  For instance,
`TonaDb.Config` will contain the connection string for the SQLite database.  By
default, this can be passed on the command line as `--db-conn-string` or as an
environment variable as `DB_CONN_STRING`.  We will see this be used later.

Your `Config` data type should generally contain `Tona*.Config` data types, as
well as any of your own configuration options you would like to pick up from
the environment.

Tonatona needs to be told how to parse your `Config` data type from the
available command line flags and environment variables.  The `HasParser` class is
used for this.  The following is a simple example of this, for when your
`Config` just contains `Tona*.Config` data types:

```haskell ignore
instance HasParser Config where
  parser = Config
      <$> parser
      <*> parser
```

Tonatona also requires a little bit of boilerplate code.  You must help
Tonatona figure out how to get the `TonaDb.Config` from your `Config`.  This is
done with the `TonaDb.HasConfig` class.  This code should be very simple to
write:

```haskell ignore
instance HasConfig Config TonaDb.Config where
  config = tonaDb
```

Now that we have all the easy code working, it is time to actually write your application!

First, we need to create a table definition.  The following creates a table to
hold blog posts.  It will have 3 columns: `id`, `author_name`, and `contents`.

Creating a table definition is a requirement for using
[persistent](http://hackage.haskell.org/package/persistent), which is what
`tonatona-persistent-sqlite` is using internally.  This is not a requirement for
Tonatona in general, just the `tonatona-persistent-sqlite` package.

```haskell ignore
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


$(share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    BlogPost
      authorName Text
      contents   Text

      deriving Show
    |]
 )
```

Next, do some DB operations in `app` function:

```haskell ignore
import Database.Persist (insert_)

app :: RIO Config ()
app = do
  -- Tonatona.Logger plugin enables to use logger functions without any configurations.
  TonaLogger.logInfo $ display ("This is a skeleton for tonatona project" :: Text)
  TonaLogger.logDebug $ display ("Migrating DB..." :: Text)
  TonaDb.runMigrate migrateAll
  TonaLogger.logDebug $ display ("Running DB query..." :: Text)
  TonaDb.run $ do
    -- By using 'lift', any plugins are available in @TonaDb.run@.
    lift $
      TonaLogger.logInfo $ display $
        ("This log is called inside of `TonaDb.run`" :: Text)
    insert_ $ BlogPost "Mr. Foo Bar" "This is an example blog post"
  TonaLogger.logInfo $ display ("Successfully inserted a blog post!" :: Text)
```

A summary of the steps you need to take is as follows:

1.  Create a `Config` data type for your application.  If you want
    to use multiple plugins, just have your data types hold multiple `Tona*.Config`.

1.  Create a `HasParser` instance for your `Config` data type.

1.  Create a `Tona*.HasConfig` instance for each of the plugins you are using.

1.  Actually write your application using the `RIO Config ()` monad.

## Available Plugins

Tonatona has many plugins available.  Here are the plugins provided in this repository.

*   [tonatona-persistent-postgresql](./tonatona-persistent-postgresql/README.md)

    [![Hackage](https://img.shields.io/hackage/v/tonatona-persistent-postgresql.svg)](https://hackage.haskell.org/package/tonatona-persistent-postgresql)
    [![Stackage LTS](http://stackage.org/package/tonatona-persistent-postgresql/badge/lts)](http://stackage.org/lts/package/tonatona-persistent-postgresql)
    [![Stackage Nightly](http://stackage.org/package/tonatona-persistent-postgresql/badge/nightly)](http://stackage.org/nightly/package/tonatona-persistent-postgresql)

    Provide access to a PostgreSQL database through the [persistent](http://hackage.haskell.org/package/persistent) library.

*   [tonatona-persistent-sqlite](./tonatona-persistent-sqlite/README.md)

    [![Hackage](https://img.shields.io/hackage/v/tonatona-persistent-sqlite.svg)](https://hackage.haskell.org/package/tonatona-persistent-sqlite)
    [![Stackage LTS](http://stackage.org/package/tonatona-persistent-sqlite/badge/lts)](http://stackage.org/lts/package/tonatona-persistent-sqlite)
    [![Stackage Nightly](http://stackage.org/package/tonatona-persistent-sqlite/badge/nightly)](http://stackage.org/nightly/package/tonatona-persistent-sqlite)

    Provide access to a SQLite database through the [persistent](http://hackage.haskell.org/package/persistent) library.

*   [tonatona-logger](./tonatona-logger/README.md)

    [![Hackage](https://img.shields.io/hackage/v/tonatona-logger.svg)](https://hackage.haskell.org/package/tonatona-logger)
    [![Stackage LTS](http://stackage.org/package/tonatona-logger/badge/lts)](http://stackage.org/lts/package/tonatona-logger)
    [![Stackage Nightly](http://stackage.org/package/tonatona-logger/badge/nightly)](http://stackage.org/nightly/package/tonatona-logger)

    Provide a way to log to the console at runtime using [monad-logger](http://hackage.haskell.org/package/monad-logger).

*   [tonatona-servant](./tonatona-servant/README.md)

    [![Hackage](https://img.shields.io/hackage/v/tonatona-servant.svg)](https://hackage.haskell.org/package/tonatona-servant)
    [![Stackage LTS](http://stackage.org/package/tonatona-servant/badge/lts)](http://stackage.org/lts/package/tonatona-servant)
    [![Stackage Nightly](http://stackage.org/package/tonatona-servant/badge/nightly)](http://stackage.org/nightly/package/tonatona-servant)

    Provide an easy way to run a [servant server](http://hackage.haskell.org/package/servant-server).

## Additional Features

Tonatona has the additional general features that apply to every plugin:

-   Make the end-user write a little boilerplate code up front in order to provide
    ease-of-use when writing their own business logic.

-   End users should be able to use many plugins without any configuration code
    or setup code.  Plugins should be configured to get configuration options
    from environment variables, command line flags, etc.  Plugins should be
    configured with reasonable defaults.

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
