{-|
  This module provides 'withGlobalPostgresqlConn', a function that parameterizes
  a global reference to a PostgreSQL database, 'dbConn'. This is intended for
  use in test code that does not need a connection pool and simply needs to
  execute functions against a database. It also provides 'postgresOptions', an
  @envparse@ 'Env.Parser' for 'PostgresOptions', which makes it easy to
  configure the PostgreSQL connection via the environment.

  If you use @hspec@, you should wrap your top-level test execution with
  'withGlobalPostgresqlConn' to properly set the global connection during the
  dynamic scope of your tests and ensure the database is rolled back after each
  test, then use 'runDB' in your tests to utilize the global connection.

  Example:

  @
  main :: 'IO' ()
  main = do
    opts <- 'Env.parse' ('Env.header' "test suite") 'postgresOptions'
    'withGlobalPostgresqlConn' opts $
      'Hspec.hspec' spec

  spec :: 'Spec'
  spec = 'Hspec.describe' "GET /foo" $ do
    'Hspec.it' "should produce a Foo" $ 'dbExample' $ do
      let foo = Foo { fooBar = "baz" }
      fooId <- 'Persist.insert' foo
      result <- getFoo (FooId 1)
      result `​'Hspec.shouldBe'​` foo
  @
-}
module Genesis.Test.Persist
  ( -- * Managing the global database connection
    runDB
  , runDBCommit
  , dbExample
  , dbConn
  , withGlobalPostgresqlConn
  -- * Postgres options envparse parser
  , PostgresOptions(..)
  , postgresOptions
  ) where

import qualified Control.Monad.Persist as Persist
import qualified Database.Persist.Postgresql as PG
import qualified Genesis.Test.Hspec as Hspec

import Control.Exception (bracket_)
import Control.Monad.Base (liftBase)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseOp_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Genesis.Persist (PostgresOptions(..), postgresOptions, withPostgresqlConn)
import GHC.Stack (HasCallStack)
import System.IO.Unsafe (unsafePerformIO)

dbConnRef :: IORef (Maybe Persist.SqlBackend)
dbConnRef = unsafePerformIO (newIORef Nothing)
{-# NOINLINE dbConnRef #-}

{-|
  Low-level access to the global database connection. If the global connection
  does not exist (that is, you are not in the dynamic extent of a call to
  'withGlobalPostgresqlConn'), this will raise an exception.
-}
dbConn :: HasCallStack => MonadBaseControl IO m => m Persist.SqlBackend
dbConn = fromMaybe (error "dbConn: connection does not exist") <$> liftBase (readIORef dbConnRef)

{-|
  Runs a computation that may interact with a database using the global database
  context, then rolls back the transaction once the computation has completed.
  This is intended to be wrapped around a single test case to create a
  self-contained test that interacts with the database.

  If you are using @hspec@, the 'dbExample' helper may be more useful and
  concise, but this function is provided for uses that fall outside of simple
  @hspec@ examples.
-}
runDB :: (MonadIO m, MonadBaseControl IO m) => Persist.SqlPersistT m a -> m a
runDB x = Persist.runPersistT (x <* Persist.transactionUndo) =<< dbConn

{-|
  Like 'runDB', except that the transaction is commited after running instead of
  rolled back. You should avoid this in test code to avoid creating tests that
  dependent on the database state, but it can be useful to run migrations, for
  example.
-}
runDBCommit :: (MonadIO m, MonadBaseControl IO m) => Persist.SqlPersistT m a -> m a
runDBCommit x = Persist.runPersistT (x <* Persist.transactionSave) =<< dbConn

{-|
  A helper function that combines 'Hspec.example' with 'runDB'. This can be used
  with 'Hspec.it' to create a test case which has access to the database within
  its body:

  @
  spec = 'Hspec.describe' "the database" $
    'Hspec.it' "holds records" $ 'dbExample' $ do
      ...
  @

  When using this function, you should most likely also use "Genesis.Test.Hspec"
  instead of "Test.Hspec" to avoid unnecessarily lifting of assertions.
-}
dbExample :: Persist.SqlPersistT IO () -> Hspec.Expectation
dbExample = Hspec.example . runDB

{-|
  Parameterizes the global database connection, 'dbConn', within the dynamic
  extent of its execution. The connection is started within a transaction.
-}
withGlobalPostgresqlConn :: MonadBaseControl IO m => PostgresOptions -> m a -> m a
withGlobalPostgresqlConn opts = liftBaseOp_ $ \action ->
  runNoLoggingT $ withPostgresqlConn opts $ \conn -> do
    oldConn <- liftBase $ readIORef dbConnRef
    liftBase $ bracket_ (writeIORef dbConnRef (Just conn)) (writeIORef dbConnRef oldConn) $ do
      PG.connBegin conn (PG.getStmtConn conn) -- start a new transaction
      action
