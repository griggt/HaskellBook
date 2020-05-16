module Strings where

-- exercise #1
notThe :: String -> Maybe String
notThe x = case x of 
    "the" -> Nothing
    _     -> Just x

replaceThe :: String -> String
replaceThe = unwords . map theToA . map notThe . words 
  where
    theToA Nothing  = "a"
    theToA (Just x) = x


-- for subsequent exercises
isVowel :: Char -> Bool
isVowel = flip elem "aeiouAEIOU"

-- exercise #2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go False 0 . words 
  where 
      go _     acc []     = acc

    --   go False acc (x:xs) = case x of
    --                           "the" -> go True acc xs
    --                           _     -> go False acc xs
    --   go True  acc (x:xs) = if isVowel (head x)
    --                         then go False (acc+1) xs
    --                         else go False acc xs

      go the acc (x:xs)
        | (not the) && (x == "the")  = go True acc xs
        | the && isVowel (head x)    = go False (acc+1) xs
        | otherwise                  = go False acc xs


-- exercise #3
countVowels :: String -> Integer
countVowels = toInteger . length . extractVowels
  where
    extractVowels ""     = ""
    extractVowels (x:xs)
      | isVowel x = x : extractVowels xs
      | otherwise = extractVowels xs

-- exercise: validate the word

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord xs
  | numVowels > numConsonants = Nothing
  | otherwise                 = Just (Word' xs)
  where
    numVowels = countVowels xs
    numConsonants = toInteger (length xs) - numVowels
