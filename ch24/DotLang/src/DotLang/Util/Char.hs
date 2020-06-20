module DotLang.Util.Char
  ( ignoreCase
  , endOfLine
  ) where

import Control.Applicative ((<|>))
import Data.Char (toLower, toUpper)
import Text.Parser.Char (CharParsing, char, string)
import Text.Parser.Combinators (eof)

-- parse a string without regard to case
ignoreCase :: CharParsing m => String -> m String
ignoreCase s = sequenceA $ zipWith (<|>) (char . toUpper <$> s) (char . toLower <$> s)

-- parse end-of-line markers for common text files
endOfLine :: CharParsing m => m [Char]
endOfLine = string "\r\n" <|> (pure <$> char '\n') <|> ("" <$ eof)
