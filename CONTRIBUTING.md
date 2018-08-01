# Contributing

Feel free to open an issue or a pull request adding new plugins to Tonatona.
New plugins are generally Haskell modules that start with `tonatona-`.

You can find a couple examples that already exist in this repository:

-   [tonatona-db-postgresql](./tonatona-db-postgresql/)
-   [tonatona-email-sendmail](./tonatona-email-sendmail/)
-   [tonatona-environment](./tonatona-environment/)
-   [tonatona-logger](./tonatona-logger/)
-   [tonatona-servant](./tonatona-servant/)

## Guidelines

Here are some guidelines for creating new Tonatona modules.

-   In general, making the end-user write a little extra boilerplate code is
    not a bad thing.  This okay as long as the code is simple and easy to
    copy-paste.

-   In the `Shared` data type, try to only use normal values, instead of
    functions. In general, it should not be a common interface, but values that
    are shared.

    For instance, the
    [`Shared`](https://github.com/arow-oss/tonatona/blob/3a2d9dd5d6ef95b51cb63605cdf7400a7dc9e136/tonatona-db-postgresql/src/Tonatona/Db/Postgresql.hs#L95-L97)
    data type in the `tonatona-db-postgresql` package contains a `Pool
    SqlBackend`, NOT a function like `ReaderT SqlBackend m a -> m a` that
    contains a `Pool SqlBackend` internally.

    (However, the `Pool` data type
    [contains](https://github.com/arow-oss/tonatona/issues/11#issue-340102261)
    functions internally.  This is okay, but if possible try to use values that
    are serializable and don't contain functions internally.)

-   For modules that don't need configuration data at application startup time,
    DON'T declare the `Config` type and `HasConfig` class. If the application
    doesn't need data at runtime, then don't declare the `Shared` type and
    `HasShared` class.

    For instance, the `tonatona-email-sendmail` class doesn't define either
    `Config` or `Shared`.

-   In general, we want to define the `Config` and `Shared` types, and the
    `HasConfig` and `HasShared` types on the child modules. For example,
    instead of having `Config` and `HasConfig` defined in a `Tonatona.Db`
    module, we instead define them in the `Tonatona.Db.Postgresql` module.

    If you think you need `Config` and `Shared` to be defined in the parent
    module and shared between the child modules, it may be worthwhile to
    consider that you actually need to provide more configuration options in
    your parent module and just not have any child modules.

-   Similarly to the last point, if the child module needs to run commands in a
    separate monad, then that monad should be defined in the child module
    itself, and not in a parent module.

    For example, `TonaDbM` is defined separately in both
    [`Tonatona.Db.Postgresql`](https://github.com/arow-oss/tonatona/blob/3a2d9dd5d6ef95b51cb63605cdf7400a7dc9e136/tonatona-db-postgresql/src/Tonatona/Db/Postgresql.hs#L35-L36)
    and
    [`Tonatona.Db.Sqlite`](https://github.com/arow-oss/tonatona/blob/3a2d9dd5d6ef95b51cb63605cdf7400a7dc9e136/tonatona-db-sqlite/src/Tonatona/Db/Sqlite.hs#L36-L37).

-   If you need to decide at runtime whether to use one child module or the
    other, then it is possible to use a sum-type to hold two different child
    modules.

    In this case you will need to use `error` in the definition of `HasShared`.
    You will also need to use a wrapper function for the functions defined in
    each child module.

    It is ONLY appropriate to use `error` in the definition of `HasShared` when
    you are picking at runtime which of the child modules to use.

    There is an example of this in the `tonatona-sample` package.  Look at the
    [`SharedDb`](https://github.com/arow-oss/tonatona/blob/3a2d9dd5d6ef95b51cb63605cdf7400a7dc9e136/tonatona-sample/src/Tonatona/Sample.hs#L141-L149)
    sum-type as well as the
    [`sharedDbRun`](https://github.com/arow-oss/tonatona/blob/3a2d9dd5d6ef95b51cb63605cdf7400a7dc9e136/tonatona-sample/src/Tonatona/Sample.hs#L151-L159)
    function.

-   If you have added a new plugin, consider adding an example of using it to
    the [tonatona-sample](./tonatona-sample/) application.
