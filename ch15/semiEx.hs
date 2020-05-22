module SemiExercises where

import Control.Monad
import Data.Monoid
import Test.QuickCheck hiding (Failure, Success)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

------------------------------------------------------------------
-- Exercise #1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

test_exercise1 :: IO ()
test_exercise1 = quickCheck (semigroupAssoc :: TrivAssoc)

------------------------------------------------------------------
-- Exercise #2

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

type IdentAssoc = Identity String -> Identity String -> Identity String -> Bool

test_exercise2 :: IO ()
test_exercise2 = quickCheck (semigroupAssoc :: IdentAssoc)

------------------------------------------------------------------
-- Exercise #3

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = (Two (x <> x') (y <> y'))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary

type TwoTest = Two String [Int]
type TwoAssoc = TwoTest -> TwoTest -> TwoTest -> Bool

test_exercise3 :: IO ()
test_exercise3 = quickCheck (semigroupAssoc :: TwoAssoc)

------------------------------------------------------------------
-- Exercise #4

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = (Three (x <> x') (y <> y') (z <> z'))

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftM3 Three arbitrary arbitrary arbitrary

type ThreeTest = Three [Int] String (Maybe String)
type ThreeAssoc = ThreeTest -> ThreeTest -> ThreeTest -> Bool

test_exercise4 :: IO ()
test_exercise4 = quickCheck (semigroupAssoc :: ThreeAssoc)

------------------------------------------------------------------
-- Exercise #5

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four p q r s) <> (Four p' q' r' s') = (Four (p <> p') (q <> q') (r <> r') (s <> s'))

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = liftM4 Four arbitrary arbitrary arbitrary arbitrary

type FourTest = Four [Bool] [Int] [Float] [Char]
type FourAssoc = FourTest -> FourTest -> FourTest -> Bool

test_exercise5 :: IO ()
test_exercise5 = quickCheck (semigroupAssoc :: FourAssoc)

------------------------------------------------------------------
-- Exercise #6

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  _              <> BoolConj False = BoolConj False
  BoolConj False <> _              = BoolConj False
  BoolConj True  <> BoolConj True  = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj False, BoolConj True]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

test_exercise6 :: IO ()
test_exercise6 = quickCheck (semigroupAssoc :: BoolConjAssoc)

------------------------------------------------------------------
-- Exercise #7

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  _              <> BoolDisj True  = BoolDisj True
  BoolDisj True  <> _              = BoolDisj True
  BoolDisj False <> BoolDisj False = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj False, BoolDisj True]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

test_exercise7 :: IO ()
test_exercise7 = quickCheck (semigroupAssoc :: BoolDisjAssoc)

------------------------------------------------------------------
-- Exercise #8

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst _ <> Fst y  = Fst y
  Fst _ <> Snd y  = Snd y
  Snd x <> _      = Snd x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [liftM Fst arbitrary, liftM Snd arbitrary]

type OrTest = Or Int Bool
type OrAssoc = OrTest -> OrTest -> OrTest -> Bool

test_exercise8 :: IO ()
test_exercise8 = quickCheck (semigroupAssoc :: OrAssoc)

------------------------------------------------------------------
-- Exercise #9

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

-- instance Arbitrary (Combine a b) where
--   arbitrary = do
--     x <- arbitrary
--     return x
  -- arbitrary = arbitrary >>= return

combineAssoc :: (Eq b, Semigroup b) 
             => Fun a b 
             -> Fun a b 
             -> Fun a b 
             -> a
             -> Bool
combineAssoc (Fn f1) (Fn f2) (Fn f3) x =
  unCombine (m1 <> (m2 <> m3)) x == unCombine ((m1 <> m2) <> m3) x
    where m1 = Combine f1
          m2 = Combine f2
          m3 = Combine f3

type CombA = Int
type CombB = Sum Int
type CombTestFun = Fun CombA CombB
type CombAssocProp = CombTestFun -> CombTestFun -> CombTestFun -> CombA -> Bool

test_exercise9 :: IO ()
test_exercise9 = quickCheck (combineAssoc :: CombAssocProp)

------------------------------------------------------------------
-- Exercise #10

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

-- sample usage
--  f = Comp $ \n  -> n + (Sum 1)
--  g = Comp $ \n  -> n - (Sum 2)
--  unComp (f <> g) $ 3      --> Sum {getSum = 2}
--  unComp (g <> f) $ 3      --> Sum {getSum = 2}
--  unComp (f <> f) $ 3      --> Sum {getSum = 5}
--  unComp (g <> g) $ 3      --> Sum {getSum = -1}

-- instance Arbitrary (Comp a) where
--   arbitrary = do
--     x <- arbitrary
--     return x

compAssoc :: (Eq a, Semigroup a)
          => Fun a a
          -> Fun a a
          -> Fun a a
          -> a
          -> Bool
compAssoc (Fn f1) (Fn f2) (Fn f3) x =
  unComp (m1 <> (m2 <> m3)) x == unComp ((m1 <> m2) <> m3) x
    where m1 = Comp f1
          m2 = Comp f2
          m3 = Comp f3

type CompA = Sum Int
type CompTestFun = Fun CompA CompA
type CompAssocProp = CompTestFun -> CompTestFun -> CompTestFun -> CompA -> Bool

test_exercise10 :: IO ()
test_exercise10 = quickCheck (compAssoc :: CompAssocProp)

------------------------------------------------------------------
-- Exercise #11

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure x <> Failure y = Failure (x <> y)
  Success x <> _         = Success x
  _         <> Success y = Success y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [liftM Failure arbitrary, liftM Success arbitrary]

type ValTest = Validation String Int
type ValAssoc = ValTest -> ValTest -> ValTest -> Bool

test_exercise11 :: IO ()
test_exercise11 = quickCheck (semigroupAssoc :: ValAssoc)

main = do
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

-------
doAllTests :: IO ()
doAllTests = do
  test_exercise1
  test_exercise2
  test_exercise3
  test_exercise4
  test_exercise5
  test_exercise6
  test_exercise7
  test_exercise8
  test_exercise9
  test_exercise10
  test_exercise11
