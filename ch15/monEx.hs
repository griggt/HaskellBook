module MonoidExercises where

import Control.Monad
import Data.Monoid
import Test.QuickCheck hiding (Failure, Success)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

------------------------------------------------------------------
-- Exercise #1

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

test_exercise1 :: IO ()
test_exercise1 = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

------------------------------------------------------------------
-- Exercise #2

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
   mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

type IdentAssoc = Identity String -> Identity String -> Identity String -> Bool

test_exercise2 :: IO ()
test_exercise2 = do
    quickCheck (semigroupAssoc :: IdentAssoc)
    quickCheck (monoidLeftIdentity :: (Identity String) -> Bool)
    quickCheck (monoidRightIdentity :: (Identity String) -> Bool)

------------------------------------------------------------------
-- Exercise #3

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = (Two (x <> x') (y <> y'))

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM2 Two arbitrary arbitrary

type TwoTest = Two String [Int]
type TwoAssoc = TwoTest -> TwoTest -> TwoTest -> Bool

test_exercise3 :: IO ()
test_exercise3 = do
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: TwoTest -> Bool)
    quickCheck (monoidRightIdentity :: TwoTest -> Bool)

------------------------------------------------------------------
-- Exercise #4

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  _              <> BoolConj False = BoolConj False
  BoolConj False <> _              = BoolConj False
  BoolConj True  <> BoolConj True  = BoolConj True

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj False, BoolConj True]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

test_exercise4 :: IO ()
test_exercise4 = do
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

------------------------------------------------------------------
-- Exercise #5

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  _              <> BoolDisj True  = BoolDisj True
  BoolDisj True  <> _              = BoolDisj True
  BoolDisj False <> BoolDisj False = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj False, BoolDisj True]

instance Monoid BoolDisj where
  mempty = BoolDisj False

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

test_exercise5 :: IO ()
test_exercise5 = do
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

------------------------------------------------------------------
-- Exercise #6

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty

combineLeftIdentity :: (Eq b, Monoid b) => Fun a b -> a -> Bool
combineLeftIdentity (Fn f) x = 
  unCombine (m <> mempty) x == unCombine m x
  where m = Combine f

combineRightIdentity :: (Eq b, Monoid b) => Fun a b -> a -> Bool
combineRightIdentity (Fn f) x = 
  unCombine (mempty <> m) x == unCombine m x
    where m = Combine f

type CombA = Int
type CombB = Sum Int
type CombTestFun = Fun CombA CombB
type CombIdentProp = CombTestFun -> CombA -> Bool

test_exercise6 :: IO ()
test_exercise6 = do
  quickCheck (combineLeftIdentity :: CombIdentProp)
  quickCheck (combineRightIdentity :: CombIdentProp)

------------------------------------------------------------------
-- Exercise #7

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id

type CompA = Sum Int
type CompTestFun = Fun CompA CompA
type CompIdentProp = CompTestFun -> CompA -> Bool

compLeftIdentity :: (Eq a, Monoid a) => Fun a a -> a -> Bool
compLeftIdentity (Fn f) x =
  unComp (m <> mempty) x == unComp m x
    where m = Comp f

compRightIdentity :: (Eq a, Monoid a) => Fun a a -> a -> Bool
compRightIdentity (Fn f) x = 
  unComp (mempty <> m) x == unComp m x
    where m = Comp f

test_exercise7 :: IO ()
test_exercise7 = do
  quickCheck (compLeftIdentity :: CompIdentProp)
  quickCheck (compRightIdentity :: CompIdentProp)

------------------------------------------------------------------
-- Exercise #8

newtype Mem s a = Mem { runMem :: s -> (a, s) }

-- TODO: can this function be simplified? (it can be made a lambda)
instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ go
    where go s = let (x, s') = f s
                     (x', s'') = g s'
                 in (x <> x', s'')

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ (,) mempty
  -- mempty = Mem $ \s -> (mempty, s)

f' = Mem $ \s -> ("hi", s + 1)
g' = Mem $ \s -> ("ya", s * 2)

main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0 
  print $ rmleft == runMem f' 0

-- should output:
--  ("hi", 1)
--  ("hi", 1)
--  ("", 0)
--  True
--  True

memAssoc :: (Eq s, Eq a, Monoid a)
         => Fun s (a, s)
         -> Fun s (a, s)
         -> Fun s (a, s)
         -> s
         -> Bool
memAssoc (Fn f1) (Fn f2) (Fn f3) x =
  runMem (m1 <> (m2 <> m3)) x == runMem ((m1 <> m2) <> m3) x
    where m1 = Mem f1
          m2 = Mem f2
          m3 = Mem f3

memLeftIdentity :: (Eq s, Eq a, Monoid a) => Fun s (a, s) -> s -> Bool
memLeftIdentity (Fn f) x =
  runMem (m <> mempty) x == runMem m x
    where m = Mem f

memRightIdentity :: (Eq s, Eq a, Monoid a) => Fun s (a, s) -> s -> Bool
memRightIdentity (Fn f) x =
  runMem (mempty <> m) x == runMem m x
    where m = Mem f

type MemS = Int
type MemA = String
type MemTestFun = Fun MemS (MemA, MemS)
type MemIdentProp = MemTestFun -> MemS -> Bool
type MemAssocProp = MemTestFun -> MemTestFun -> MemTestFun -> MemS -> Bool

test_exercise8 :: IO ()
test_exercise8 = do
  quickCheck (memAssoc :: MemAssocProp)
  quickCheck (memLeftIdentity :: MemIdentProp)
  quickCheck (memRightIdentity :: MemIdentProp)

--------------------------------------------------
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
