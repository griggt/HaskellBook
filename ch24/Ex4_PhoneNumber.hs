module PhoneNumber where

import Control.Applicative
import Data.Char
import Data.Foldable
import Text.Trifecta

type NumeringAreaPlan = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumeringAreaPlan Exchange LineNumber
  deriving (Eq, Show)

-- should handle at minimum these formats
--     xxx-xxx-xxxx
--     xxxxxxxxxx
--     (xxx) xxx-xxxx
--     1-xxx-xxx-xxxx

-- Numbering area plan
--   xxx-
--   xxx
--   (xxx) + <space>     (use `parens`)
--   1-xxx-

-- This leaves a remaining parse of
--     xxx-xxxx
--     xxxxxxx
--     xxx-xxxx
--     xxx-xxxx

--  Exchange
--     xxx-
--     xxx

--- LineNumber
---    xxxx

-- This approach may admit other (nonstandard) styles as well. Investigate.

parsePhone :: Parser PhoneNumber
parsePhone = liftA3 PhoneNumber areaCode exchange lineNumber

areaCode :: Parser NumeringAreaPlan
areaCode = (parens (decimalLen 3))
       <|> (string "1-" *> decimalLen 3 <* optional (char '-'))
       <|> ((decimalLen 3) <* optional (char '-'))

exchange :: Parser Exchange
exchange = (decimalLen 3) <* optional (char '-')

lineNumber :: Parser LineNumber
lineNumber = decimalLen 4

decimalLen :: Int -> Parser Int
decimalLen n = foldl' (\x d -> 10 * x + digitToInt d) 0 <$> (count n digit)
