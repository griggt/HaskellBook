module MonadExercises where

import Control.Applicative (liftA)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--------------------------------------------------------------
-- exercise #1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

-- testing
instance Arbitrary (Nope a) where
  arbitrary = elements [NopeDotJpg]

instance EqProp (Nope a) where
  (=-=) = eq

test_exercise1 :: IO ()
test_exercise1 = do
  let testTypes :: Nope (String, Int, Bool)
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ applicative testTypes
  quickBatch $ monad testTypes

--------------------------------------------------------------
-- exercise #2
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight y) = PRight y
  fmap f (PLeft x)  = PLeft (f x)

instance Applicative (BahEither b) where
  pure = PLeft
  PRight e <*> _ = PRight e
  PLeft f  <*> x = fmap f x  

instance Monad (BahEither b) where
  PRight e >>= _ = PRight e
  PLeft x  >>= f = f x

-- testing
instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = oneof [liftA PLeft arbitrary, liftA PRight arbitrary]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

test_exercise2 :: IO ()
test_exercise2 = do
  let testTypes :: BahEither String (Int, Bool, Char)
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ applicative testTypes
  quickBatch $ monad testTypes

--------------------------------------------------------------
-- exercise #3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> x = fmap f x

instance Monad Identity where
  (Identity x) >>= f = f x

-- testing
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = liftA Identity arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

test_exercise3 :: IO ()
test_exercise3 = do
  let testTypes :: Identity (String, Int, Bool)
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ applicative testTypes
  quickBatch $ monad testTypes

--------------------------------------------------------------
-- exercise #4
data List a = Nil | Cons a (List a) deriving (Eq, Show)

fromL :: [a] -> List a
fromL = foldr Cons Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' . fmap f $ as

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

instance Monad List where
  Nil >>= _ = Nil
  xs  >>= f = flatMap f xs

-- testing
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = fmap fromL arbitrary

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

test_exercise4 :: IO ()
test_exercise4 = do
  let testTypes :: List (String, Int, Bool)
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ applicative testTypes
  quickBatch $ monad testTypes
