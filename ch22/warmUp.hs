module WarmUp where

import Control.Applicative (liftA2)
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledA :: [Char] -> ([Char], [Char])
tupledA = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  c <- cap
  r <- rev
  return (c, r)

-- TODO any way to do this without the lambdas?
tupled'' :: [Char] -> ([Char], [Char])
tupled'' =
  cap >>=
    \c ->
      rev >>=
        \r ->
          return (c, r)
