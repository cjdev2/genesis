{-|
  This module provides utilities for using PostgreSQL with
  "Control.Monad.Persist" and @persistent@.
-}
module Genesis.Persist
  ( PostgresOptions(..)
  , postgresOptions
  , withPostgresqlPool
  , withPostgresqlConn
  ) where

import qualified Database.Persist.Postgresql as PG
import qualified Env

import Control.Monad ((<=<))
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Persist (SqlBackend)
import Data.Monoid ((<>))
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text.Conversions (fromText, toText, unUTF8)
import GHC.Generics (Generic)

{-|
  Connection options needed to connect to PostgreSQL. You can use
  'postgresOptions' to parse these options from the environment.
-}
data PostgresOptions = PostgresOptions
  { host :: Text
  , port :: Int
  , user :: Text
  , dbName :: Text
  , password :: Text
  } deriving (Eq, Show, Generic)

{-|
  An @envparse@ 'Env.Parser' that parses 'PostgresOptions' from the environment,
  looking for the environment variables @PG_HOST@, @PG_PORT@, @PG_USER@,
  @PG_USER@, @PG_DB_NAME@, and @PG_PASSWORD@. All of them are optional except
  for @PG_DB_NAME@.
-}
postgresOptions :: (Env.AsEmpty e, Env.AsUnread e, Env.AsUnset e) => Env.Parser e PostgresOptions
postgresOptions = PostgresOptions
    <$> Env.var (Env.str <=< Env.nonempty) "PG_HOST" (helpDef "localhost" "host of postgres database")
    <*> Env.var (Env.auto <=< Env.nonempty) "PG_PORT" (helpDef 5432 "port of postgres database")
    <*> Env.var (Env.str <=< Env.nonempty) "PG_USER" (helpDef "postgres" "user to connect to postgres as")
    <*> Env.var (Env.str <=< Env.nonempty) "PG_DB_NAME" (Env.help "postgres database name to connect to")
    <*> Env.var Env.str "PG_PASSWORD" (helpDef "" "password to connect to postgres with")
  where helpDef def msg = Env.def def <> Env.help (msg ++ " (default: " ++ show def ++ ")")

{-|
  Like 'PG.withPostgresqlPool' from "Database.Persist.Postgresql", except using
  'PostgresOptions'.
-}
withPostgresqlPool :: (MonadBaseControl IO m, MonadLogger m, MonadIO m)
                   => PostgresOptions -- ^ Options to connect to the database.
                   -> Int -- ^ Number of connections to be kept open in the pool.
                   -> (Pool SqlBackend -> m a) -- ^ Action to be executed that uses the connection pool.
                   -> m a
withPostgresqlPool opts = PG.withPostgresqlPool (pgConnString opts)

{-|
Like 'PG.withPostgresqlConn' from "Database.Persist.Postgresql", except using
'PostgresOptions'.
-}
withPostgresqlConn :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => PostgresOptions -> (SqlBackend -> m a) -> m a
withPostgresqlConn opts = PG.withPostgresqlConn (pgConnString opts)

pgConnString :: PostgresOptions -> PG.ConnectionString
pgConnString PostgresOptions { host, port, user, dbName, password } =
  unUTF8 . fromText $ "host=" <> host <> " port=" <> toText (show port) <> " user=" <> user
                   <> " dbname=" <> dbName <> " password=" <> password
