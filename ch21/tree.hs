module Tree where

import Control.Applicative (liftA, liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf x)     = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty        = mempty
  foldMap f (Leaf x)     = f x
  foldMap f (Node l x r) = (foldMap f l) <> (f x) <> (foldMap f r)
  -- TODO also implement foldr; not quite so natural

instance Traversable Tree where
  traverse _ Empty        = pure Empty
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Node l x r) = liftA3 Node (traverse f l) (f x) (traverse f r)

-- testing
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = 
    frequency [ (1, pure Empty)
              , (2, liftA Leaf arbitrary)
              , (2, liftA3 Node arbitrary arbitrary arbitrary)]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

test_tree :: IO ()
test_tree = do
  let testTypes :: Tree (Int, Int, [Int])
      testTypes = undefined
  quickBatch $ functor testTypes
  quickBatch $ traversable testTypes
