# Contributing

Feel free to open an issue or a pull request adding new plugins to Tonatona.
New plugins are generally Haskell modules that start with `tonatona-`.

You can find a couple examples that already exist in this repository:

-   [tonatona-persistent-postgresql](./tonatona-persistent-postgresql/)
-   [tonatona-email-sendmail](./tonatona-email-sendmail/)
-   [tonatona-environment](./tonatona-environment/)
-   [tonatona-logger](./tonatona-logger/)
-   [tonatona-servant](./tonatona-servant/)

## Guidelines

Here are some guidelines for creating new Tonatona modules.

-   In general, making the end-user write a little extra boilerplate code is
    not a bad thing.  This okay as long as the code is simple and easy to
    copy-paste.

-   If you have added a new plugin, consider adding an example of using it to
    the [tonatona-sample](./tonatona-sample/) application.
