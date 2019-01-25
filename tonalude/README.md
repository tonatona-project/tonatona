# The tonalude library

A standard library for Tonatona framework.

## Prelude replacement for Tonatona framework

The Tonalude module works as a prelude replacement for [Tonatona framework](https://github.com/tonatona-project/tonatona), providing more functionality and types out of the box than the standard prelude (such as common data types like ByteString and Text), as well as removing common "gotchas", like partial functions and lazy I/O.
Most of the functions and types are imported from [rio](https://github.com/commercialhaskell/rio), but Tonalude customizes them to be more suitable for using with Tonatona.

The goal of Tonalude is not to be a **general** alternative to Prelude,
but to be a Prelude alternative **only for Tonatona**.
