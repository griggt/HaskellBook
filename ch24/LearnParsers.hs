module LearnParsers where

import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser Char
one = char '1' <* eof

-- read one then fail
one' = one >> stop

-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2' <* eof

-- read two then fail
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

----------------------------------------------------------------
-- Exercise #2
p123 :: String -> Result String
p123 s = parseString parser mempty s
  -- NB important to place the most specific case first, as the
  -- first successful alternative will be the one used.
  where parser = string "123" <|> string "12" <|> string "1"

----------------------------------------------------------------
-- Exercise #3
-- TODO is this solution correct? It accepts lots more valid
-- input than does `p123`.
p123c :: String -> Result String
p123c s = parseString parser mempty s
  where parser = some $ char '1' <|> char '2' <|> char '3'

-- This does not work for "12"
p123c' :: String -> Result String
p123c' s = parseString parser mempty s
  where
    parser = parse123 <|> parse12
    parse123 = do
      a <- char '1'
      b <- char '2'
      c <- char '3'
      pure [a, b, c]
    parse12 = do
      a <- char '1'
      b <- char '2'
      pure [a, b]
