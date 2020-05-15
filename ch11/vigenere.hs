module Vigenere where

import Data.Char
import Debug.Trace

-- Shift an alphabetic character C by a fixed integer N with a base B
shift :: Char -> Int -> Char -> Char
shift base n c = chr $ b + mod (n + ord c - b) 26
  where b = ord base

padOffset :: String -> Int
padOffset pad = ord (toUpper . head $ pad) - ord 'A'

-- the direction we are encoding
--  id     = forward encoding
--  negate = reverse encding (decoding)
type Direction = (Int -> Int)

-- returns a tuple: (encoded character, remaining pad string)
encodeWith :: Direction -> Char -> String -> (Char, String)
encodeWith dir c pad
  | isUpper c = shiftFrom 'A'
  | isLower c = shiftFrom 'a'
  | otherwise = (c, pad)
  where shiftFrom b = (shift b (dir . padOffset $ pad) c, drop 1 pad)

encode = encodeWith id
decode = encodeWith negate

type Coder = (Char -> String -> (Char, String))

_vigenere :: Coder -> String -> String -> String
_vigenere coder key msg = v' (cycle key) msg
  where
      v' _ "" = ""
      v' pad (x:xs) = e : (v' p xs)
        where (e, p) = coder x pad

vigenere :: String -> String -> String
vigenere = _vigenere encode

unvigenere :: String -> String -> String
unvigenere = _vigenere decode
