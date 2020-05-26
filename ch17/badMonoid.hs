module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = False

instance EqProp Bull where
  (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)


-----------------------------------------------
-- applicative :: ( Show a, Show (m a), Show (m (a -> b))
--   , Show (m (b -> c)), Applicative m
--   , CoArbitrary a, EqProp (m a)
--   , EqProp (m b), EqProp (m c)
--   , Arbitrary a, Arbitrary b
--   , Arbitrary (m a)
--   , Arbitrary (m (a -> b))
--   , Arbitrary (m (b -> c)))
--   => m (a, b, c) -> TestBatch

testApplicative :: IO ()
testApplicative = do
  quickBatch $ applicative [("b", "w", 1)]
