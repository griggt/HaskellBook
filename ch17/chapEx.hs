module ChapterExercises where

import Control.Monad
import Data.Monoid (Sum)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------------------------------------
-- exercise #1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f1 f2) <*> (Pair x1 x2) = Pair (f1 x1) (f2 x2)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = liftM2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

test_exercise1 :: IO ()
test_exercise1 = do
  let testTypes :: Pair (Int, String, Bool)
      testTypes = undefined
    in quickBatch $ applicative testTypes

-----------------------------------------------------------------
-- exercise #2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Monoid a) => Applicative (Two a) where
  pure x = Two mempty x
  (Two x f) <*> (Two y n) = Two (x <> y) (f n)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

test_exercise2 :: IO ()
test_exercise2 = do
  let testTypes :: Two [Int] (Int, String, Bool)
      testTypes = undefined
    in quickBatch $ applicative testTypes

-----------------------------------------------------------------
-- exercise #3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three x y f) <*> (Three x' y' n) = Three (x <> x') (y <> y') (f n)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

test_exercise3 :: IO ()
test_exercise3 = do
  let testTypes :: Three [Int] String (Bool, Int, String)
      testTypes = undefined
    in quickBatch $ applicative testTypes

-----------------------------------------------------------------
-- exercise #4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' x f1 f2) <*> (Three' x' n1 n2) = Three' (x <> x') (f1 n1) (f2 n2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftM3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

test_exercise4 :: IO ()
test_exercise4 = do
  let testTypes :: Three' [Int] (Bool, Int, String)
      testTypes = undefined
    in quickBatch $ applicative testTypes

-----------------------------------------------------------------
-- exercise #5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z n) = Four x y z (f n)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four x y z f) <*> (Four x' y' z' n) = Four (x <> x') (y <> y') (z <> z') (f n)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = liftM4 Four arbitrary arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

test_exercise5 :: IO ()
test_exercise5 = do
  let testTypes :: Four [Int] (Sum Int) String (Int, Char, String)
      testTypes = undefined
    in quickBatch $ applicative testTypes

-----------------------------------------------------------------
-- exercise #6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z n) = Four' x y z (f n)

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' x y z f) <*> (Four' x' y' z' n) = Four' (x <> x') (y <> y') (z <> z') (f n)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = liftM4 Four' arbitrary arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

test_exercise6 :: IO ()
test_exercise6 = do
  let testTypes :: Four' String (Bool, Char, Int)
      testTypes = undefined
    in quickBatch $ applicative testTypes
