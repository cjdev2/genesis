{-# LANGUAGE TemplateHaskell #-}

import qualified Env
import qualified Spec

import Control.Monad.Logger (runNoLoggingT)
import Genesis.Persist.Migrate (runMigrations)
import Genesis.Test.Hspec (hspec)
import Genesis.Test.Persist

main :: IO ()
main = do
  opts <- Env.parse (Env.header "genesis-test test suite") postgresOptions
  runNoLoggingT $ withGlobalPostgresqlConn opts $ do
    runDBCommit $(runMigrations "test-suite/migrations")
    hspec Spec.spec
