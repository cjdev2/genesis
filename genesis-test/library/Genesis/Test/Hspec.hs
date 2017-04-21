{-|
  This module re-exports everything from "Test.Hspec", except with modified
  versions of the runner and expectations that operate over an arbitrary monad
  transformer stack with 'IO' at the base. This is especially useful in
  conjunction with 'Genesis.Test.Persist.dbExample' from "Genesis.Test.Persist",
  which runs an @hspec@ example in a monad that can interact with a database.
-}
module Genesis.Test.Hspec
  ( module Test.Hspec
  , hspec
  , expectationFailure
  , shouldBe
  , shouldSatisfy
  , shouldStartWith
  , shouldEndWith
  , shouldContain
  , shouldMatchList
  , shouldReturn
  , shouldNotBe
  , shouldNotSatisfy
  , shouldNotContain
  , shouldNotReturn
  , shouldThrow
  ) where

import qualified Test.Hspec as Hspec

import Control.Exception.Lifted (Exception, try)
import Control.Monad (unless)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Proxy (Proxy(..))
import Data.Typeable (typeRep)
import GHC.Stack (HasCallStack)
import Test.Hspec hiding
  ( hspec
  , expectationFailure
  , shouldBe
  , shouldSatisfy
  , shouldStartWith
  , shouldEndWith
  , shouldContain
  , shouldMatchList
  , shouldReturn
  , shouldNotBe
  , shouldNotSatisfy
  , shouldNotContain
  , shouldNotReturn
  , shouldThrow
  )

hspec :: MonadBase IO m => Spec -> m ()
hspec x = liftBase $ Hspec.hspec x

expectationFailure :: (HasCallStack, MonadBase IO m) => String -> m ()
expectationFailure x = liftBase $ Hspec.expectationFailure x

expectTrue :: (HasCallStack, MonadBase IO m) => String -> Bool -> m ()
expectTrue msg b = unless b (expectationFailure msg)

shouldBe :: (HasCallStack, Show a, Eq a, MonadBase IO m) => a -> a -> m ()
shouldBe x y = liftBase $ Hspec.shouldBe x y

shouldSatisfy :: (HasCallStack, Show a, MonadBase IO m) => a -> (a -> Bool) -> m ()
shouldSatisfy x y = liftBase $ Hspec.shouldSatisfy x y

shouldStartWith :: (HasCallStack, Show a, Eq a, MonadBase IO m) => [a] -> [a] -> m ()
shouldStartWith x y = liftBase $ Hspec.shouldStartWith x y

shouldEndWith :: (HasCallStack, Show a, Eq a, MonadBase IO m) => [a] -> [a] -> m ()
shouldEndWith x y = liftBase $ Hspec.shouldEndWith x y

shouldContain :: (HasCallStack, Show a, Eq a, MonadBase IO m) => [a] -> [a] -> m ()
shouldContain x y = liftBase $ Hspec.shouldContain x y

shouldMatchList :: (HasCallStack, Show a, Eq a, MonadBase IO m) => [a] -> [a] -> m ()
shouldMatchList x y = liftBase $ Hspec.shouldMatchList x y

shouldNotBe :: (HasCallStack, Show a, Eq a, MonadBase IO m) => a -> a -> m ()
shouldNotBe x y = liftBase $ Hspec.shouldNotBe x y

shouldNotSatisfy :: (HasCallStack, Show a, MonadBase IO m) => a -> (a -> Bool) -> m ()
shouldNotSatisfy x y = liftBase $ Hspec.shouldNotSatisfy x y

shouldNotContain :: (HasCallStack, Show a, Eq a, MonadBase IO m) => [a] -> [a] -> m ()
shouldNotContain x y = liftBase $ Hspec.shouldNotContain x y

shouldReturn :: (HasCallStack, Show a, Eq a, MonadBase IO m) => m a -> a -> m ()
shouldReturn action x = action >>= (`shouldBe` x)

shouldNotReturn :: (HasCallStack, Show a, Eq a, MonadBase IO m) => m a -> a -> m ()
shouldNotReturn action x = action >>= (`shouldNotBe` x)

-- impl modified from Test.Hspec.Expectations.shouldThrow
shouldThrow :: forall e a m. (HasCallStack, Exception e, MonadBaseControl IO m) => m a -> Selector e -> m ()
shouldThrow action p = try action >>= \case
    Right _ -> expectationFailure $
      "did not get expected exception: " ++ exceptionType
    Left e -> flip expectTrue (p e) $
      "predicate failed on expected exception: " ++ exceptionType ++ " (" ++ show e ++ ")"
  where exceptionType = show $ typeRep (Proxy @e) -- a string repsentation of the expected exception's type
