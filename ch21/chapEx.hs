module ChapterExercises where

import Control.Applicative (liftA, liftA2, liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

----------------------------------------------------
-- exercise #1
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

-- testing
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftA Identity arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

test_exercise1 :: IO ()
test_exercise1 = do
  let testTypes :: Identity (Int, Int, [Int])
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ traversable testTypes

----------------------------------------------------
-- exercise #2
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant x) = pure (Constant x)

-- testing
instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = liftA Constant arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

test_exercise2 :: IO ()
test_exercise2 = do
  let testTypes :: Constant String (Int, Int, [Int])
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ traversable testTypes

----------------------------------------------------
-- exercise #3
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

-- testing
instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = frequency [(1, return Nada),
                         (3, liftA Yep arbitrary)]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq                        

test_exercise3 :: IO ()
test_exercise3 = do
  let testTypes :: Optional (Int, Int, [Int])
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ traversable testTypes

----------------------------------------------------
-- exercise #4
data List a = Nil | Cons a (List a) deriving (Eq, Show)

fromL :: [a] -> List a
fromL = foldr Cons Nil

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)
  -- traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs)

-- testing
instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = fmap fromL arbitrary

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

test_exercise4 :: IO ()
test_exercise4 = do
  let testTypes :: List (Int, Int, [Int])
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ traversable testTypes
  
----------------------------------------------------
-- exercise #5
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  traverse f (Three x y z) = (Three x y) <$> f z

-- testing
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

test_exercise5 :: IO ()
test_exercise5 = do
  let testTypes :: Three Int Bool (Int, Int, [Int])
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ traversable testTypes

----------------------------------------------------
-- exercise #6
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
  foldMap f (Pair _ y) = f y

instance Traversable (Pair a) where
  traverse f (Pair x y) = (Pair x) <$> f y

-- testing
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

test_exercise6 :: IO ()
test_exercise6 = do
  let testTypes :: Pair Bool (Int, Int, [Int])
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ traversable testTypes

----------------------------------------------------
-- exercise #7
-- When you have more than one value of type b, use Monoid and Applicative
-- for the Foldable and Traversable instances, respectively:
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where 
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big _ y z) = (f y) <> (f z)

instance Traversable (Big a) where
  traverse f (Big x y z) = (Big x) <$> (f y) <*> (f z)
  -- traverse f (Big x y z) = liftA2 (Big x) (f y) (f z)

-- testing
instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

test_exercise7 :: IO ()
test_exercise7 = do
  let testTypes :: Big Bool (Int, Int, [Int])
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ traversable testTypes
  
----------------------------------------------------
-- exercise #8
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger e x y z) = Bigger e (f x) (f y) (f z)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ x y z) = (f x) <> (f y) <> (f z)

instance Traversable (Bigger a) where
  traverse f (Bigger e x y z) = (Bigger e) <$> (f x) <*> (f y) <*> (f z)

-- testing
instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

test_exercise8 :: IO ()
test_exercise8 = do
  let testTypes :: Bigger Bool (Int, Int, [Int])
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ traversable testTypes
