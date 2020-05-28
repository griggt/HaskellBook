module Validation where

import Control.Monad
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e) where
  pure = Success

  Failure e <*> Failure e' = Failure (e <> e')
  Failure e <*> _ = Failure e
  Success s <*> x = fmap s x

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [liftM Failure arbitrary, liftM Success arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  let testTypes :: Validation [Int] (Int, Char, Bool)
      testTypes = undefined
    in quickBatch $ applicative testTypes
