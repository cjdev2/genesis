# genesis [![Build Status](https://travis-ci.org/cjdev/genesis.svg?branch=master)](https://travis-ci.org/cjdev/genesis)

Genesis is a project that codifies *opinionated Haskell bootstrapping* for Haskell web services. Before explaining what Genesis is, it’s important to establish what Genesis is *not*:

  1. **Genesis is not a framework.** Genesis does not attempt to be its own framework. Instead, it simply provides a set of useful libraries and glue to help them work well together. By making decisions about which libraries to use, it can provide more specialized, easier to use interfaces than the unopinionated choices of the individual libraries.

  2. **Genesis is not scaffolding.** “Scaffolding” projects are also called “templates” or “project generators”, and they simply dump a lot of code into your project when you start it. There are serious problems with scaffolding, most notably that they cannot be easily updated, so the code becomes *your* responsibility to understand and maintain. Genesis is a library, not a generator, so it can be incrementally updated just like anything else.

Genesis is a library that helps to make decisions for you, so you don’t have to spend huge amounts of time reinventing the wheel just to combine different libraries. Currently, the set of technologies Genesis is designed to support are the following:

  - [servant][] for serving RESTful HTTP endpoints.
  - [persistent][] and [esqueleto][] (with [monad-persist][]) for database queries.
  - [hspec][] for running tests.
  - [PostgreSQL][postgres] for storing data.

Currently, Genesis provides two significant features:

  1. **Database migrations.** Genesis database migrations are not written in Haskell, they are written in SQL, but they are compiled into your application using Template Haskell, so they can be run upon startup. This makes it easy to also run the migrations in a test suite.

  2. **Support for running database code in your test suite.** If you are building a web service that “owns” its database, it’s often far more trouble than its worth to mock/stub out the database in your tests. Genesis makes it easy to run a test suite against the same database with the same schema that your application runs against.

For more information, see [genesis][] and [genesis-test][] on Hackage.

[esqueleto]: https://hackage.haskell.org/package/esqueleto
[genesis]: https://hackage.haskell.org/package/genesis
[genesis-test]: https://hackage.haskell.org/package/genesis-test
[hspec]: https://hackage.haskell.org/package/hspec
[monad-persist]: https://hackage.haskell.org/package/monad-persist
[persistent]: https://hackage.haskell.org/package/persistent
[postgres]: https://www.postgresql.org
[servant]: https://hackage.haskell.org/package/servant
