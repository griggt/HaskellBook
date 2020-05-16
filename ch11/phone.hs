module Phone where

import Data.Char
import Data.List
import Data.Maybe

-- valid buttons = "1234567890*#"
type Digit = Char

-- valid presses: 1 and up
type Presses = Int

data Button = Button Digit [Char]
    deriving Show

data DaPhone = DaPhone [Button]
    deriving Show

phone :: DaPhone
phone = 
    DaPhone [
        (Button '1' "1"),
        (Button '2' "ABC2"),
        (Button '3' "DEF3"),
        (Button '4' "GHI4"),
        (Button '5' "JKL5"),
        (Button '6' "MNO6"),
        (Button '7' "PQRS7"),
        (Button '8' "TUV8"),
        (Button '9' "WXYZ9"),
        (Button '*' "^*"),
        (Button '0' "+ 0"),
        (Button '#' ".,#")
    ]

-- exercise #2
convo :: [String]
convo = [
    "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur coool haha. Ur turn",
    "OK. DO you think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps ph@(DaPhone btns) x 
  | isUpper x = (reverseTaps ph '^') ++ (taps x)
  | otherwise = taps (toUpper x)
  where
    taps c = [(digit, presses)]
      where 
        Button digit chars = head $ filter (\(Button _ xs) -> elem c xs) btns
        presses = maybe (-1) (+1) (elemIndex c chars)

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead ph = concatMap (reverseTaps ph)

-- exercise #3
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) n -> n + p) 0

-- exercise #4
freqCount :: Ord a => [a] -> [(a, Int)]
freqCount = map accum . groupBy firsts . sort . flip zip (repeat 1)
  where
      accum = foldr1 (\(x, y) (_, y') -> (x, y + y'))
      firsts x y = fst x == fst y

--  sort . flip zip (repeat 1)   -> [('a',1),('a',1),('a',1),('b',1),('b',1)]  (e.g.)
--  groupBy (\x y -> fst x == fst y)  -> [[('a',1),('a',1),('a',1)],[('b',1),('b',1)]]
--  map (foldr1 (\(x, y) (_, y') -> (x, y + y')))  -> [('a',3),('b',2)]

mostPopular :: Ord a => [a] -> a
mostPopular = fst . maximumBy freqCompare . freqCount
  where freqCompare (_, x) (_, y) = compare x y

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopular . filter isLetter

mostPopularCost :: DaPhone -> String -> Presses
mostPopularCost ph = fingerTaps . reverseTaps ph . mostPopularLetter

-- exercise #5
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . filter isLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopular . concat . (map words)
