module AnotherMaybe where

import Control.Monad
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada   <> y      = y
  x      <> Nada   = x 
  Only x <> Only y = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

-----

newtype First' a = First' { getFirst :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  First' Nada <> y = y
  x           <> _ = x

instance Monoid (First' a) where
  mempty = First' Nada

-----

-- TODO understand how liftM works here. stolen from pg 563

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [ (1, return Nada)
                        , (1, liftM Only arbitrary) ]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = liftM First' arbitrary

-----

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-----

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
