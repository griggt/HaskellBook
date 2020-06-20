module DotLang.Util.Char where

import Control.Applicative ((<|>))
import Data.Char (toLower, toUpper)
import Text.Parser.Char (CharParsing, char, string)

ignoreCase :: CharParsing m => String -> m String
ignoreCase s = sequenceA $ zipWith (<|>) (char . toUpper <$> s) (char . toLower <$> s)

endOfLine :: CharParsing m => m [Char]
endOfLine = string "\r\n" <|> (pure <$> char '\n')  -- TODO or EOF?
