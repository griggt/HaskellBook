module ListApplicative where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs) 

instance Applicative List where
  pure x = (Cons x Nil)
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  -- TODO this definition is recursive and doesn't use flatMap. sub-optimal?
  (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

fromL :: [a] -> List a
-- fromL []     = Nil
-- fromL (x:xs) = Cons x (fromL xs)
fromL = foldr Cons Nil

toL :: List a -> [a]
toL Nil         = []
toL (Cons x xs) = x : toL xs

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

-- testing

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap fromL arbitrary

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = do
  let testTypes :: List (Char, Int, String)
      testTypes = undefined
    in quickBatch $ applicative testTypes
