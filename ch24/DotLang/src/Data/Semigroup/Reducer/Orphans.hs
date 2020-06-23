{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Semigroup.Reducer.Orphans where

import Data.Semigroup.Reducer (Reducer(..))

-- trivial reducer of a semigroup to itself
instance (Semigroup a) => Reducer a a where
  unit = id
