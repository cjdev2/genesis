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

import Control.Monad.Persist (Entity(..), insert, selectList)
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
spec = describe "dbExample" $
  it "runs an Hspec example with access to the database" $ dbExample $ do
    blogId <- insert $ Blog "Alyssa’s Blog"
    let post = Post { postTitle = "First Post", postBody = "world changing post", postBlogId = blogId }
    postId <- insert post
    posts <- selectList [] []
    posts `shouldBe` [Entity postId post]
