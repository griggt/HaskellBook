{-# LANGUAGE OverloadedStrings #-}

module ParseInteger where

import Control.Applicative
import Data.Char
import Data.Foldable
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

-- Straightforward implementation -- using zipWith
base10Integer :: Parser Integer
base10Integer = do
  digitChars <- some parseDigit
  let digits = digitToNum <$> digitChars
  return (sum $ zipWith (*) (reverse digits) powersOfTen)
  where
    digitToNum d = fromIntegral $ ord d - ord '0'
    powersOfTen = [10^n | n <- [0..]]

-- But we can be more clever with folding...
base10Integer' :: Parser Integer
base10Integer' = do
  digitChars <- some parseDigit
  return (foldl' combine 0 digitChars)
  where
    digitToNum d = fromIntegral $ ord d - ord '0'  -- TODO digitToInt already exists in Data.Char
    combine acc x = acc * 10 + (digitToNum x)

-- Also handles +/- prefix
base10Integer'' :: Parser Integer
base10Integer'' = base10Integer' <|> positiveInteger <|> negativeInteger

positiveInteger :: Parser Integer
positiveInteger = char '+' *> base10Integer'

negativeInteger :: Parser Integer
negativeInteger = char '-' *> (negate <$> base10Integer')
