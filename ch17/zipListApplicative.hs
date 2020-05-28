module ZipListApplicative where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (repeat x)
  ZipList' xs <*> ZipList' ys = ZipList' $ zipWith ($) xs ys

    -- NB, can also use `id` in place of `($)` in above
    -- The base lib actually implements liftA2 instead of <*>,
    --  the default implementation of <*> is `liftA2 id`
    -- TODO is the ($) truly a like-for-like replacement, or
    --  can it fail sometimes?

-- testing

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = let (ZipList' l) = xs
            in take 3000 l
      ys' = let (ZipList' l) = ys
            in take 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = fmap ZipList' arbitrary

main :: IO ()
main = do
  let testTypes :: ZipList' (Int, Bool, String)
      testTypes = undefined
    in quickBatch $ applicative testTypes
