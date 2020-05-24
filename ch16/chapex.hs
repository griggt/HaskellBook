module ChapterExercises where

import Control.Monad
import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

--------------------------------------------------------------

data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' x)  = False' (f x)
  fmap f (True' y)   = True' (f y)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
   arbitrary = oneof [liftM False' arbitrary, liftM True' arbitrary]

type TestBSE = BoolAndSomethingElse Int -> Bool

test_bse :: IO ()
test_bse = do
  quickCheck (functorIdentity :: TestBSE)
  quickCheck (functorCompose (+1) (*2) :: TestBSE)

--------------------------------------------------------------
-- Rearrange the arguments to the type constructor
--------------------------------------------------------------

-- #1
data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a)  = First (f a)
  fmap f (Second b) = Second b

-- #2
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- #3
data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

--------------------------------------------------------------
-- Write Functor instances for the datatype
--------------------------------------------------------------

-- #1
data Quant a b = Finance | Desk a | Bloor b deriving Show

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor y) = Bloor (f y)

-- #2
data K a b = K a

instance Functor (K a) where
  fmap _ (K x) = K x

-- #4
data EvilGoateeConst a b = GoatyConst b deriving Show

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

-- #5
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (fmap f x)

-- #6
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

-- #7
data IgnoreOne f g a b = IgnoreSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoreSomething x y) = IgnoreSomething x (fmap f y)

-- #8
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

-- #9
data List a = Nil | Cons a (List a) deriving Show

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs) 

-- #10
data GoatLord a = 
    NoGoat 
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat             = NoGoat
  fmap f (OneGoat x)        = OneGoat (f x)
  fmap f (MoreGoats x y z)  = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- #11
data TalkToMe a = 
    Halt
  | Print String a
  | Read (String -> a)

instance (Show a) => Show (TalkToMe a) where
  show Halt          = "Halt"
  show (Print x y)   = "Print " ++ (show x) ++ " " ++ (show y)
  show (Read f)      = "Read <func>"
  
instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print x y) = Print x (f y)
--  fmap f (Read g)    = Read $ \s -> f (g s)
  fmap f (Read g)    = Read (f . g)
