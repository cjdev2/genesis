{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Genesis.Sample where

import qualified Env
import Control.Monad.Persist
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Logger (runStderrLoggingT)
import Data.Text (Text)
import Data.Pool (Pool, withResource)
import Database.Persist.TH
import Genesis.Persist (postgresOptions, runMigrations, PostgresOptions, withPostgresqlConn, withPostgresqlPool)
import Network.Wai.Handler.Warp (run)
import Servant

share [mkPersist sqlSettings] [persistLowerCase|
Message
  content Text
  deriving Show
|]

type Api = GetMessage :<|> PostMessage
type GetMessage = "message" :> Capture "id" MessageId :> Get '[JSON] Text
type PostMessage = "message" :> ReqBody '[JSON] Text :> Post '[JSON] MessageId

newtype AppHandler a = AppHandler (ReaderT () (PersistT SqlBackend (ExceptT ServantErr IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (), MonadError ServantErr, MonadPersist SqlBackend)

getMessage :: (MonadPersist SqlBackend m, MonadError ServantErr m) => MessageId -> m Text
getMessage msgId = do
  maybeMsg <- get msgId
  case maybeMsg of
    Nothing -> throwError err404
    Just (Message content) -> return content

postMessage :: (MonadIO m, MonadPersist SqlBackend m) => Text -> m MessageId
postMessage content = insert (Message content)

api :: ServerT Api AppHandler
api = getMessage :<|> postMessage

server :: Pool SqlBackend -> Server Api
server pool = hoistServerWithContext (Proxy :: Proxy Api) (Proxy :: Proxy '[]) appToHandler api
  where
    appToHandler :: forall a. AppHandler a -> Handler a
    appToHandler (AppHandler m) = Handler . withResource pool $ runPersistT (runReaderT m ())

application :: Pool SqlBackend -> Application
application pool = serve (Proxy :: Proxy Api) (server pool)

migrateDB :: PostgresOptions -> IO ()
migrateDB opts = runStderrLoggingT $ withPostgresqlConn opts $
  runSqlPersistT $(runMigrations "db/migrations")

main :: IO ()
main = do
  -- Don't forget to create the database before running.
  --
  -- `postgresOptions` reads from the environment:
  --   * PG_HOST
  --   * PG_PORT
  --   * PG_USER
  --   * PG_PASSWORD
  --   * PG_DB_NAME
  opts <- Env.parse (Env.header "options") postgresOptions
  migrateDB opts
  let connections = 8
  runStderrLoggingT $
    withPostgresqlPool opts connections $ \pool ->
      liftIO $ run 8080 (application pool)
