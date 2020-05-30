module ChapterExercises where

import Data.Bool (bool)

-- Write foldable instances

-------------------------------------------------------------
-- exercise #1
data Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap f (Constant x) = f x

-------------------------------------------------------------
-- exercise #2
data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ y) = f y

-------------------------------------------------------------
-- exercise #3
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

-------------------------------------------------------------
-- exercise #4
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ y z) = (f y) <> (f z)

-------------------------------------------------------------
-- exercise #5
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ x y z) = (f x) <> (f y) <> (f z)


-------------------------------------------------------------
-- Bonus exercise, write the function filterF using foldMap

filterF :: (Applicative f, Foldable t, Monoid (f a)) =>
            (a -> Bool) -> t a -> f a
filterF p = foldMap (\x -> bool mempty (pure x) (p x))

-- note that the result is polymorphic; will need to add
-- a type ascription to a concrete type to e.g. show the result

-- an example:
--   Prelude> x = filterF (> 10) [8..15] 
--   Prelude> x :: [Int]
--   [11,12,13,14,15]
--   Prelude> import Data.Vector
--   Data.Vector> x :: Vector Int
--   [11,12,13,14,15]

  -- f needs to map a to a Monoid, whose mappend will discard
  --   the undesired elements (translated to mempty)
  -- "pure" will summon an implicit Applicative instance
  -- "mempty" will summon an implicit Monoid instance