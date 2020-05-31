module SkiFree where

import Control.Applicative (liftA2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- NB the type variable n is ok kind * -> *
data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n
         , Testable (n Property)
         , Eq a
         , Eq (n a)
         , EqProp a)
        => EqProp (S n a) where
  (=-=) = eq

instance (Functor n) => Functor (S n) where
  fmap f (S xs x) = S (fmap f xs) (f x)

instance (Foldable n) => Foldable (S n) where
  foldMap f (S xs x) = (foldMap f xs) <> (f x)

instance (Traversable n) => Traversable (S n) where
  traverse f (S xs x) = liftA2 S (traverse f xs) (f x)

main :: IO ()
main = do
  -- sample (arbitrary :: Gen (S [] Int))
  let testTypes :: S [] (Int, Int, [Int])
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ foldable (undefined :: S [] (Int, Int, [Int], Int, Int))
  quickBatch $ traversable testTypes
