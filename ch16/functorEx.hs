module FunctorExercises where

import Control.Monad
import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

------------------------------------
-- exercise #1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

type TestIdentity = Identity Int -> Bool

test_exercise1 :: IO ()
test_exercise1 = do
  quickCheck (functorIdentity :: TestIdentity)
  quickCheck (functorCompose (+1) (*2) :: TestIdentity)

------------------------------------
-- exercise #2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = liftM2 Pair arbitrary arbitrary

type TestPair = Pair Int -> Bool

test_exercise2 :: IO ()
test_exercise2 = do
  quickCheck (functorIdentity :: TestPair)
  quickCheck (functorCompose (+1) (*2) :: TestPair)

------------------------------------
-- exercise #3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary

type TestTwo = Two String Int -> Bool

test_exercise3 :: IO ()
test_exercise3 = do
  quickCheck (functorIdentity :: TestTwo)
  quickCheck (functorCompose (+1) (*2) :: TestTwo)

------------------------------------
-- exercise #4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary

type TestThree = Three String Bool Int -> Bool

test_exercise4 :: IO ()
test_exercise4 = do
  quickCheck (functorIdentity :: TestThree)
  quickCheck (functorCompose (+1) (*2) :: TestThree)

------------------------------------
-- exercise #5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftM3 Three' arbitrary arbitrary arbitrary

type TestThree' = Three' String Int -> Bool

test_exercise5 :: IO ()
test_exercise5 = do
  quickCheck (functorIdentity :: TestThree')
  quickCheck (functorCompose (+1) (*2) :: TestThree')

------------------------------------
-- exercise #6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x y z r) = Four x y z (f r)

type TestFour = Four Bool String Char Int -> Bool

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = liftM4 Four arbitrary arbitrary arbitrary arbitrary

test_exercise6 :: IO ()
test_exercise6 = do
  quickCheck (functorIdentity :: TestFour)
  quickCheck (functorCompose (+1) (*2) :: TestFour)

------------------------------------
-- exercise #7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x y z r) = Four' x y z (f r)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = liftM4 Four' arbitrary arbitrary arbitrary arbitrary

type TestFour' = Four' String Int -> Bool

test_exercise7 :: IO ()
test_exercise7 = do
  quickCheck (functorIdentity :: TestFour')
  quickCheck (functorCompose (+1) (*2) :: TestFour')

main :: IO ()
main = do
  test_exercise1
  test_exercise2
  test_exercise3
  test_exercise4
  test_exercise5
  test_exercise6
  test_exercise7
