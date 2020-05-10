module Cipher where

import Data.Char

shift :: Char -> Int -> Char -> Char
shift base n c = chr $ b + mod (n + ord c - b) 26
  where b = ord base

-- TODO can I nicely rewrite encode in point-free style?

caesar :: Int -> String -> String
caesar _ ""     = ""
caesar n (x:xs) = (encode x) : caesar n xs 
  where
    encode c
      | isUpper c = shift 'A' n c
      | isLower c = shift 'a' n c
      | otherwise = c

uncaesar :: Int -> String -> String
uncaesar _ ""     = ""
uncaesar n (x:xs) = (decode x) : uncaesar n xs
  where
    decode c
      | isUpper c = shift 'A' (-n) c
      | isLower c = shift 'a' (-n) c
      | otherwise = c
