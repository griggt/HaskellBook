module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"


digits :: Int -> [Int]
digits n
  | n < 0     = digits (negate n)    -- drop minus sign for negative numbers
  | q == 0    = onesD
  | otherwise = (digits q) ++ onesD
  where (q, r) = divMod n 10
        onesD  = (:[]) r
    

wordNumber :: Int -> String
wordNumber n =
    concat . intersperse "-" . map digitToWord . digits $ n
