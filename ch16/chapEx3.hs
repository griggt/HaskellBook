{-# LANGUAGE FlexibleInstances #-}

module ChapEx2 where

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K a b = K a deriving Show

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))
