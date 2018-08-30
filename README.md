# Tonatona

[![Build Status](https://secure.travis-ci.org/arow-oss/tonatona.svg)](http://travis-ci.org/arow-oss/tonatona)
[![Hackage](https://img.shields.io/hackage/v/tonatona.svg)](https://hackage.haskell.org/package/tonatona)
[![Stackage LTS](http://stackage.org/package/tonatona/badge/lts)](http://stackage.org/lts/package/tonatona)
[![Stackage Nightly](http://stackage.org/package/tonatona/badge/nightly)](http://stackage.org/nightly/package/tonatona)

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
using a new plugin, often all you have to do is just import it!  No need to
specify configuration from within your application.

## How to use Tonatona

Using Tonatona is relatively simple. It requires declaring a few datatypes, as
well as instances for classes provided by Tonatona.

This section describes how to do this, using the
[tonatona-db-sqlite](./tonatona-db-sqlite) plugin as an example.

First, we need some language pragmas and imports:

```haskell
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.Persist.Class (insert_)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import TonaParser (FromEnv(fromEnv), Parser)
import Tonatona (Plug(init), TonaM)
import qualified Tonatona as Tona
import qualified Tonatona.Db.Sqlite as TonaDb
import Tonatona.Logger (stdoutLogger)
```

Next, we need to create a table definition.  The following creates a table to
hold blog posts.  It will have 3 columns: `id`, `author_name`, and `contents`.

Creating a table definition is a requirement for using
[persistent](http://hackage.haskell.org/package/persistent), which is what
`tonatona-db-sqlite` is using internally.  This is not a requirement for
Tonatona in general, just the `tonatona-db-sqlite` package.

```haskell
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

Next, you need to create data types called `Config` and `Shared`.  These will be
used for Tonatona:

```haskell
data Config = Config
  { configTonaDb :: TonaDb.Config
  }
  deriving (Show)

data Shared = Shared
  { sharedTonaDb :: TonaDb.Shared
  }
```

Your `Config` data type will contain configuration values that can be read in on
the command line or through environment variables.  For instance,
`TonaDb.Config` will contain the connection string for the SQLite database.  By
default, this can be passed on the command line as `--db-conn-string` or as an
environment variable as `DB_CONN_STRING`.  We will see this be used later.

Your `Config` data type should generally contain `Tona*.Config` data types, as
well as any of your own configuration options you would like to pick up from
the environment.

Your `Shared` data type will contain runtime values that will be needed by your
application.  For instance, `TonaDb.Shared` will contain a connection to the
SQLite database.  This can only be constructed at runtime and is not a value
that can be passed in through a command line flag or environment variable.

Your `Shared` data type should generally contain `Tona*.Shared` data types, as
well as any runtime values your application needs.

There are some plugins that only have either a `Config` or `Shared` data type,
but not both.

Tonatona needs to be told how to parse your `Config` data type from the
available command line flags and environment variables.  The `FromEnv` class is
used for this.  The following is a simple example of this, for when your
`Config` just contains `Tona*.Config` data types:

```haskell
instance FromEnv Config where
  fromEnv :: Parser Config
  fromEnv = Config <$> fromEnv
```

Tonatona also requires a little bit of boilerplate code.  You must help
Tonatona figure out how to get the `TonaDb.Config` from your `Config`.  This is
done with the `TonaDb.HasConfig` class.  This code should be very simple to
write:

```haskell
instance TonaDb.HasConfig Config where
  config :: Config -> TonaDb.Config
  config (Config tonaDbConf) = tonaDbConf
```

The `TonaDb.HasShared` class is similar for `TonaDb.Shared` and your `Shared`:

```
instance TonaDb.HasShared Shared where
  shared :: Shared -> TonaDb.Shared
  shared (Shared tonaDbShared) = tonaDbShared
```

Now, you have to help Tonatona figure out how to create your `Shared` data type from your `Config` data type.

Most of the Tonatona plugins expose a method called `init` that can be used to
do this easily.  Generally you just need to pass your `Config` data type to the
`init` method, along with other options.

In the following code, `TonaDb.init` will return `IO TonaDb.Shared`, which you
can wrap into your `Shared` data type.

```haskell
instance Plug Config Shared where
  init :: Config -> IO Shared
  init conf = Shared <$> TonaDb.init conf stdoutLogger
```

Now that we have all the easy code working, it is time to actually write your application!

The following is an example of using the `TonaM` monad.  You can see how
`TonaDb.runMigrate` as well as `TonaDb.run` are used, both of which return
values in the `TonaM` monad.

`TonaM` has an instance of `MonadIO`, so you can do any `IO` operation with
`liftIO` as well.

```haskell
myApp :: TonaM Config Shared ()
myApp = do
  TonaDb.runMigrate migrateAll
  TonaDb.run $ insert_ $ BlogPost "Mr. Foo Bar" "This is an example blog post"
  liftIO $ putStrLn "Successfully inserted a blog post!"
```

It is often convenient to create a type synonym for `TonaM`.  The following
uses a type synonym called `Tona`:

```haskell
type Tona = TonaM Config Shared

myApp' :: Tona ()
myApp' = myApp
```

Finally, you can use the `Tona.run` function actually run your application:

```haskell
main :: IO ()
main = Tona.run myApp'
```

A summary of the steps you need to take is as follows:

1.  Create a `Config` and `Shared` data type for your application.  If you want
    to use multiple plugins, just have your data types hold multiple
    `Tona*.Config` and `Tona*.Shared` types.

1.  Create a `FromEnv` instance for your `Config` data type.

1.  Create a `Tona*.HasConfig` and `Tona*.HasShared` instance for each of the plugins you are using.

1.  Create a `Plug` instance to show how to create your `Shared` type from your `Config` type.

1.  Actually write your application using the `TonaM` monad.

1.  Run your application with the `Tona.run` function.

## Available Plugins

Tonatona has many plugins available.  Here are the plugins provided in this repository.

*   [tonatona-db-postgresql](./tonatona-db-postgresql/README.md)

    [![Hackage](https://img.shields.io/hackage/v/tonatona-db-postgresql.svg)](https://hackage.haskell.org/package/tonatona-db-postgresql)
    [![Stackage LTS](http://stackage.org/package/tonatona-db-postgresql/badge/lts)](http://stackage.org/lts/package/tonatona-db-postgresql)
    [![Stackage Nightly](http://stackage.org/package/tonatona-db-postgresql/badge/nightly)](http://stackage.org/nightly/package/tonatona-db-postgresql)

    Provide access to a PostgreSQL database through the [persistent](http://hackage.haskell.org/package/persistent) library.

*   [tonatona-db-sqlite](./tonatona-db-sqlite/README.md)

    [![Hackage](https://img.shields.io/hackage/v/tonatona-db-sqlite.svg)](https://hackage.haskell.org/package/tonatona-db-sqlite)
    [![Stackage LTS](http://stackage.org/package/tonatona-db-sqlite/badge/lts)](http://stackage.org/lts/package/tonatona-db-sqlite)
    [![Stackage Nightly](http://stackage.org/package/tonatona-db-sqlite/badge/nightly)](http://stackage.org/nightly/package/tonatona-db-sqlite)

    Provide access to a SQLite database through the [persistent](http://hackage.haskell.org/package/persistent) library.

*   [tonatona-email-sendmail](./tonatona-email-sendmail/README.md)

    [![Hackage](https://img.shields.io/hackage/v/tonatona-email-sendmail.svg)](https://hackage.haskell.org/package/tonatona-email-sendmail)
    [![Stackage LTS](http://stackage.org/package/tonatona-email-sendmail/badge/lts)](http://stackage.org/lts/package/tonatona-email-sendmail)
    [![Stackage Nightly](http://stackage.org/package/tonatona-email-sendmail/badge/nightly)](http://stackage.org/nightly/package/tonatona-email-sendmail)

    Provide a way to easily send email directly by using `sendmail`.

*   [tonatona-environment](./tonatona-environment/README.md)

    [![Hackage](https://img.shields.io/hackage/v/tonatona-environment.svg)](https://hackage.haskell.org/package/tonatona-environment)
    [![Stackage LTS](http://stackage.org/package/tonatona-environment/badge/lts)](http://stackage.org/lts/package/tonatona-environment)
    [![Stackage Nightly](http://stackage.org/package/tonatona-environment/badge/nightly)](http://stackage.org/nightly/package/tonatona-environment)

    Provide a way to figure out at runtime whether we are in a `development`
    environment, `prodution` environment, or `testing` environment.

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

-   It should be possible to switch plugins just by rewriting the import statements.

    For instance, it should be possible to go from using `tonatona-db-sqlite` to
    `tonatona-db-postgresql` just by changing the import statement

    `import qualified Tonatona.Db.Sqlite as TonaDb`

    to

    `import qualified Tonatona.Db.Postgresql as TonaDb`

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
