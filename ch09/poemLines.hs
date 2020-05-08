module PoemLines where

-- warm up exercise, #1

myWords :: String -> [String]
myWords s 
  | s == ""   = []
  | otherwise = first : rest
    where first = takeWhile (/= ' ') s
          rest = myWords . dropWhile (== ' ') . dropWhile (/= ' ') $ s

-- william blake, #2

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines s
  | s == ""   = []
  | otherwise = first : rest
    where first = takeWhile (/= '\n') s
          rest = myLines . dropWhile (== '\n') . dropWhile (/= '\n') $ s


shouldEqual = 
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]


main :: IO ()
main = 
    print $
    "Are they equal? "
    ++ show (myLines sentences == shouldEqual)

-- exercise #3, generic function

breakOn :: Char -> String -> [String]
breakOn c s
  | s == ""   = []
  | otherwise = first : rest
    where first = takeWhile (/= c) s
          rest = breakOn c . dropWhile (== c) . dropWhile (/= c) $ s
