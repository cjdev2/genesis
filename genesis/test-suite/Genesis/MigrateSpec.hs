{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Genesis.MigrateSpec (spec) where

import qualified Test.Hspec as Hspec

import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Persist (Entity(..), SqlPersistT, insert, runSqlPersistT, selectList)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.TH (mkPersist, persistLowerCase, sqlSettings)
import Test.Hspec hiding (shouldBe)

import Genesis.Persist (runMigrations)

mkPersist sqlSettings [persistLowerCase|
Blog
  name Text
  deriving Eq Show

Post
  title Text
  body Text
  blogId BlogId
  deriving Eq Show
|]

runSqlite :: (MonadBaseControl IO m, MonadUnliftIO m) => SqlPersistT (NoLoggingT m) a -> m a
runSqlite x = runNoLoggingT $ withSqliteConn ":memory:" (runSqlPersistT ($(runMigrations "test-suite/migrations") >> x))

shouldBe :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
shouldBe x y = liftIO $ Hspec.shouldBe x y

spec :: Spec
spec = describe "runMigrations" $
  it "executes all SQL migrations in the specified directory" $ example $ runSqlite $ do
    blogId <- insert $ Blog "Alyssa’s Blog"
    let post = Post { postTitle = "First Post", postBody = "world changing post", postBlogId = blogId }
    postId <- insert post
    posts <- selectList [] []
    posts `shouldBe` [Entity postId post]
