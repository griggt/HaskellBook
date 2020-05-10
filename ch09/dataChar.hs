module DataChar where

import Data.Char

-- exercise 2
allupper :: String -> String
allupper = filter isUpper

-- exercise 3
titleCase :: String -> String
titleCase ""     = ""
titleCase (x:xs) = (toUpper x) : xs

-- exercise 4
allCaps :: String -> String
allCaps ""     = ""
allCaps (x:xs) = (toUpper x) : allCaps xs

-- exercise 5
firstInit :: String -> Char
firstInit s = toUpper . head $ s

-- point free
firstInit' :: String -> Char
firstInit' = toUpper . head
