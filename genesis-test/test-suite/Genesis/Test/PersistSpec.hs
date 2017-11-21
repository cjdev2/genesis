{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Genesis.Test.PersistSpec (spec) where

import Control.Monad.Persist (Entity(..), insert, insert_, selectList)
import Data.Text (Text)
import Database.Persist.TH (mkPersist, persistLowerCase, sqlSettings)
import Genesis.Test

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

spec :: Spec
spec = do
  describe "runDB" $ do
    it "rolls back when the action completes succesfully" $ example $ do
      runDB . insert_ $ Blog "Alyssa’s Blog"
      runDB (selectList [] []) `shouldReturn` ([] :: [Entity Blog])

    it "rolls back when the action fails with an exception" $ example $ do
      shouldThrow
        (runDB $ do
          insert_ $ Blog "Alyssa’s Blog"
          error "failure")
        (errorCall "failure")
      runDB (selectList [] []) `shouldReturn` ([] :: [Entity Blog])

  describe "dbExample" $
    it "runs an Hspec example with access to the database" $ dbExample $ do
      blogId <- insert $ Blog "Alyssa’s Blog"
      let post = Post { postTitle = "First Post", postBody = "world changing post", postBlogId = blogId }
      postId <- insert post
      posts <- selectList [] []
      posts `shouldBe` [Entity postId post]
