module TryTry where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

type NumberResult = Either Rational Double

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = double

parseNumber :: Parser NumberResult
parseNumber =
  try (Left <$> parseFraction)
  <|> (Right <$> parseDecimal)
