module LogFile where

import Control.Applicative
import Data.Char (digitToInt, isSpace)
import Data.List (foldl', intercalate)
import Data.Map (Map)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (TimeOfDay(..))
import Test.QuickCheck
import Text.Trifecta
import qualified Data.Map.Strict as Map

type Activity = String

data LogA = Map Day [LogEntry]
  deriving (Eq, Show)

data Log = Log [LogLine]
  deriving Show

-- instance Show Log where
--   show (Log ls) = concat $ map ((++"|") . roundtrip) ls

data LogEntry = LogEntry TimeOfDay Activity
  deriving (Eq, Show)

data LogLine =
    Entry LogEntry
  | DayLine Day
  | BlankLine
  | CommentLine String
  deriving (Eq, Show)

roundtrip :: LogLine -> String
roundtrip BlankLine = ""
roundtrip (CommentLine c) = "--" ++ c
roundtrip (DayLine d) = "# " ++ formatTime defaultTimeLocale "%0Y-%m-%d" d
roundtrip (Entry (LogEntry t a)) = formatTime defaultTimeLocale "%H:%M" t ++ " " ++ a

-- TODO dashes in activity text (requires tokenization?)
-- TODO gather LogLine data into a Log data structure
-- TODO time spent computation
-- TODO overall aggregation
-- TODO daily average aggregation

wholeLog :: Parser Log
wholeLog = Log <$> many logLine

logLine :: Parser LogLine
logLine = logEntryLine <|> dayLine <|> try commentLine <|> blankLine

blankLine :: Parser LogLine
blankLine = many spaceNotNewline >> lineEnd >> pure BlankLine

commentLine :: Parser LogLine
commentLine = many spaceNotNewline >> string "--" >> CommentLine . concat <$> some word <* lineEnd

dayLine :: Parser LogLine
dayLine = char '#' >> some spaceNotNewline >> DayLine <$> date <* trailingEnd

logEntryLine :: Parser LogLine
logEntryLine = Entry <$> logEntry <* trailingEnd

logEntry :: Parser LogEntry
logEntry = liftA2 LogEntry time activity

trailingEnd :: Parser ()
trailingEnd = try trailingWhitespace <|> (skipOptional trailingComment)

-- TODO fix trailing comment handling. The initial whitespace before
-- starting a trailing comment should be swallowed up
trailingComment :: Parser String
-- trailingComment = some spaceNotNewline >> string "--" >> concat <$> some word <* lineEnd
trailingComment = many spaceNotNewline >> string "--" >> concat <$> some word <* lineEnd

trailingWhitespace :: Parser ()
trailingWhitespace = some spaceNotNewline >> lineEnd

date :: Parser Day
date = do
  year  <- decimalLen 4
  char '-'
  month <- decimalLen 2
  char '-'
  day   <- decimalLen 2
  return $ fromGregorian (toInteger year) month day

time :: Parser TimeOfDay
time = do
  hour <- decimalLen 2
  char ':'
  min  <- decimalLen 2
  return $ TimeOfDay hour min 0

activity :: Parser String
activity = (many spaceNotNewline) *> (concat <$> (some word))

word :: Parser String
word = some (noneOf "-\n")

spaceNotNewline :: Parser Char
spaceNotNewline = satisfy (liftA2 (&&) isSpace ('\n' /=))

lineEnd :: Parser ()
lineEnd = choice [skipSome newline, eof]

decimalLen :: Int -> Parser Int
decimalLen n = foldl' (\x d -> 10 * x + digitToInt d) 0 <$> (count n digit)

-----

main :: IO ()
main = do
  file <- readFile "test.log"
  let logdata = lines file
  let parsed = doParse <$> logdata
  mapM_ (putStrLn . show) parsed
  where
    doParse x = parseString logLine mempty x

-- TODO for some reason this is introducing a log of extra BlankLine records
--      in the wrong spot, and missing altogether the actual blank line
--      towards the top
main2 :: IO ()
main2 = do
  file <- readFile "test.log"
  let parsed = doParse file
  mapM_ (putStrLn . show) parsed
  where
    doParse x = parseString wholeLog mempty x
