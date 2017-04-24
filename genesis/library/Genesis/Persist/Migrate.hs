{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-|
  This module provides functions for creating and running migrations written as
  plain SQL files. By using 'runMigrations', the migrations will be compiled
  directly into your Haskell application using Template Haskell, so the files
  will not need to be present in the eventual runtime environment.

  For more information, see the documentation for 'runMigrations'.
-}
module Genesis.Persist.Migrate (runMigrations, runMigrations') where

import qualified Data.Text.IO as T

import Control.Monad (filterM, forM, forM_, unless, when)
import Control.Monad.Logger (MonadLogger, logDebugNS, logInfoNS)
import Control.Monad.Persist (MonadSqlPersist, insert_, rawExecute, runMigrationSilent, selectList, transactionSave)
import System.Directory (doesDirectoryExist, doesFileExist, doesPathExist, listDirectory)
import System.FilePath ((</>))
import Data.FileEmbed (makeRelativeToProject)
import Data.List (isSuffixOf, sort)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Conversions (toText)
import Database.Persist.Sql (Entity(..))
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

share [mkPersist sqlSettings, mkMigrate "migrateSchema"] [persistLowerCase|
SchemaMigration
  name FilePath
  deriving Eq Show
|]

{-|
  Compiles a set of .sql files in a particular directory into your application,
  then runs them against a database at runtime. This function is implemented
  with Template Haskell so that it can read the migration files at compile-time,
  but it semantically has the type
  @('MonadLogger' m, 'MonadSqlPersist' m) => 'FilePath' -> m ()@. Migrations
  will be executed in order based on their filename, according to 'sort'.

  The 'FilePath' provided should be relative to your /project root/, and it will
  detect all files within the immediate directory (that is, /not/ in
  subdirectories) with the suffix @.sql@.

  Example:

  @
  main :: IO ()
  main = 'Control.Monad.Logger.runStderrLoggingT' $ withSqliteConn ":memory:" ('Control.Monad.Persist.runSqlPersistT' $('runMigrations' "db/migrations"))
  @
-}
runMigrations :: FilePath -> Q Exp
runMigrations dir = do
  migrationsDir <- makeRelativeToProject dir
  migrationsDirExists <- runIO $ doesPathExist migrationsDir
  unless migrationsDirExists $
    fail $ "No such directory ‘" ++ dir ++ "’ exists at root of project.\n        (Looking at ‘" ++ migrationsDir ++ "’)."

  migrationsDirIsDir <- runIO $ doesDirectoryExist migrationsDir
  unless migrationsDirIsDir $
    fail $ "The file at ‘" ++ dir ++ "’ in root of project is not a directory.\n        (Looking at ‘" ++ migrationsDir ++ "’)."

  allFiles <- runIO $ listDirectory migrationsDir
  nonDirectoryFiles <- runIO $ filterM (doesFileExist . (migrationsDir </>)) allFiles
  let migrationFiles = sort $ filter (isSuffixOf ".sql") nonDirectoryFiles
  forM_ migrationFiles (addDependentFile . (migrationsDir </>))
  when (null migrationFiles) $
    reportWarning $ "No migrations (files ending in .sql) in ‘" ++ dir ++ "’ at root of project.\n      (Looking in ‘" ++ migrationsDir ++ "’.)\n"

  migrations <- runIO $ forM migrationFiles $ \path -> (path,) <$> T.readFile (migrationsDir </> path)
  [| runMigrations' migrations |]

{-|
  The low-level API that underlies 'runMigrations'. It is unlikely that you will
  need to use this.

  Unlike 'runMigrations', this function does not use Template Haskell.
  Migrations are passed as a list of tuples, where the first element is the name
  of the migration (which must be unique) and the second element is the SQL to
  be run. The migrations will be run in the order they are provided, from left
  to right.
-}
runMigrations' :: (MonadLogger m, MonadSqlPersist m) => [(FilePath, Text)] -> m ()
runMigrations' allMigrations = do
  messages <- runMigrationSilent migrateSchema
  forM_ messages (logDebugNS "SQL")
  transactionSave
  existingMigrations <- map (schemaMigrationName . entityVal) <$> selectList [] []
  let newMigrations = filter (\(path, _) -> path `notElem` existingMigrations) allMigrations
  forM_ newMigrations $ \(path, sql) -> do
    logInfoNS "migrate" $ "migrating ‘" <> toText path <> "’"
    rawExecute sql []
    insert_ (SchemaMigration path)
    transactionSave
