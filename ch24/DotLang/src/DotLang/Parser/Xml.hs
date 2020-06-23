module DotLang.Parser.Xml
  (
    Tag
  , Element(..)
  , Content(..)
  , Reference(..)
  , CharData
  , Name
  , AttrName
  , AttrValue(..)
  , AttrValueChar(..)
  , Attribute(..)
  , QuoteChar
  , Comment(..)
  , lexElement
  , parseElement
  )
  where

import Control.Applicative ((<|>), many, some)
import Data.Char (chr)
import Data.CharSet (CharSet)
import Text.Parser.Char (CharParsing, char, noneOf, notChar, oneOfSet, string)
import Text.Parser.Combinators (between, manyTill, notFollowedBy, option, try, (<?>))
import Text.Parser.Token (TokenParsing, decimal, hexadecimal, symbolic, token, whiteSpace)
import qualified Data.CharSet as CS

----------------------------------------------------------------------------------------------
lexElement :: CharParsing m => m [String]
lexElement = many $ lexTag <|> lexNonTag

lexTag :: CharParsing m => m String
lexTag = (\a b c -> a : b ++ (pure c))
  <$> (char '<') <*> (many $ noneOf "<>") <*> (char '>')  -- TODO is > actually permitted?

lexNonTag :: CharParsing m => m String
lexNonTag = some (noneOf "<>")
----------------------------------------------------------------------------------------------

parseElement :: TokenParsing m => m Element
parseElement = element

type Tag = String
data Element = Element Tag [Attribute] [Content] (Maybe Tag)
  deriving (Eq, Show)

-- element	      ::=   	EmptyElemTag | STag content ETag
-- STag	          ::=   	'<' Name (S Attribute)* S? '>'
-- ETag	          ::=   	'</' Name S? '>'
-- EmptyElemTag	  ::=   	'<' Name (S Attribute)* S? '/>'
-- S	            ::=   	(#x20 | #x9 | #xD | #xA)+

element :: TokenParsing m => m Element
element = try emptyElement <|> contentElement

elementHead :: TokenParsing m => m ([Content] -> (Maybe Tag) -> Element)
elementHead = Element <$> name <*> option [] (try attributes) <* whiteSpace

emptyElement :: TokenParsing m => m Element
emptyElement = char '<' *> elt <* string "/>"
  where elt = elementHead <*> (pure []) <*> (pure Nothing)

-- TODO verify the name is the same on start and end tags
contentElement :: TokenParsing m => m Element
contentElement = elt
  -- do   -- TODO FIXME very buggy, doesn't admit valid xml; use Errable instead of MonadFail? or `unexpected`?
  --   (Element stag _ _ (Just etag)) <- elt
  --   if (stag /= etag)
  --   then fail "unmatched start and end tags"
  --   else elt
  where
    start = char '<' *> elementHead <* char '>'
    end   = string "</" *> (Just <$> name) <* char '>'
    elt   = start <*> content <*> end

-- content	      ::=   	CharData? ((element | Reference | Comment) CharData?)*
data Content =
    ContentCharData  CharData
  | ContentElement   Element
  | ContentReference Reference
  | ContentComment   Comment
  deriving (Eq, Show)

content :: TokenParsing m => m [Content]
content = many contentItem

-- TODO sequencing, where do I need `try`?
contentItem :: TokenParsing m => m Content
contentItem = try (ContentElement   <$> element)
          <|> try (ContentReference <$> reference)
          <|> try (ContentComment   <$> comment)
          <|>     (ContentCharData  <$> charData)
          -- TODO also comments

-- Reference	    ::=   	EntityRef | CharRef
-- CharRef	      ::=   	'&#' [0-9]+ ';'
-- 			               | '&#x' [0-9a-fA-F]+ ';'
-- EntityRef	    ::=   	'&' Name ';'
data Reference =
    CharRef Char
  | EntityRef Name
  deriving (Eq)

instance Show Reference where
  showsPrec prec (CharRef c) = showsPrec prec c
  showsPrec prec (EntityRef e) = showsPrec prec e

reference :: TokenParsing m => m Reference
reference = charRef <|> entityRef

charRef :: TokenParsing m => m Reference
charRef = string "&#" *> (CharRef . chr . fromInteger <$> (decimal <|> hexadecimal)) <* char ';'

entityRef :: TokenParsing m => m Reference
entityRef = char '&' *> (EntityRef <$> name) <* char ';'

-- CharData	      ::=   	[^<&]*
type CharData = String

charData :: (CharParsing m) => m CharData
charData = some (noneOf "<&") <?> "CharData"

type Name = String
-- Name	          ::=   	NameStartChar (NameChar)*
-- NameStartChar	::=   	":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6]
--                         | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF]
--                         | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF]
--                         | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
-- NameChar	      ::=   	NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
name :: (TokenParsing m) => m Name
name = token $ (:) <$> nameStartChar <*> (many nameChar)

nameStartChar :: (CharParsing m) => m Char
nameStartChar = oneOfSet nameStartCharSet <?> "NameStartChar"

nameChar :: (CharParsing m) => m Char
nameChar = oneOfSet nameCharSet <?> "NameChar"

nameStartCharSet :: CharSet
nameStartCharSet = CS.fromList $ ":_" ++ ['A'..'Z'] ++ ['a'..'z']
                    ++ ['\xc0'  ..'\xd6'  ] ++ ['\xd8'  ..'\xf6'  ] ++ ['\xf8'  ..'\x2ff' ]
                    ++ ['\x370' ..'\x37d' ] ++ ['\x37f' ..'\x1fff'] ++ ['\x200c'..'\x200d']
                    ++ ['\x2070'..'\x218f'] ++ ['\x2c00'..'\x2fef'] ++ ['\x3001'..'\xd7ff']
                    ++ ['\xf900'..'\xfdcf'] ++ ['\xfdf0'..'\xfffd'] ++ ['\x10000'..'\xeffff']

nameNonStartCharSet :: CharSet
nameNonStartCharSet = CS.fromList $ "-.\xb7" ++ ['0'..'9'] ++ ['\x300'..'\x36f'] ++ ['\x203f'..'\x2040']

nameCharSet :: CharSet
nameCharSet = nameStartCharSet `CS.union` nameNonStartCharSet

-- Attribute	    ::=   	Name Eq AttValue
-- Eq	            ::=   	S? '=' S?
type AttrName = String
data Attribute = Attribute AttrName AttrValue
  deriving (Eq, Show)

attributes :: (TokenParsing m) => m [Attribute]
attributes = many $ try attribute

attribute :: (TokenParsing m) => m Attribute
attribute = Attribute <$> name <* symbolic '=' <*> attrvalue

-- AttValue	      ::=   	'"' ([^<&"] | Reference)* '"'
-- 			               |  "'" ([^<&'] | Reference)* "'"
data AttrValueChar =
    AChar Char
  | ARef  Reference
  deriving Eq

instance Show AttrValueChar where
  showsPrec prec (AChar c) = showsPrec prec c
  showsPrec prec (ARef x) = showsPrec prec x

type QuoteChar = Char
data AttrValue = AttrValue [AttrValueChar] QuoteChar
  deriving Eq

instance Show AttrValue where
  show (AttrValue xs q) = [q] ++ (concatMap f xs) ++ [q]
    where
      f (AChar c) = [c]
      f (ARef (CharRef c)) = [c]
      f (ARef (EntityRef er)) = '&' : er ++ ";"

attrvalue :: (TokenParsing m) => m AttrValue
attrvalue = quoted '"'  (many $ qvalue (noneOf "<&\""))
        <|> quoted '\'' (many $ qvalue (noneOf "<&'"))
  where
    quoted q p = token $ AttrValue <$> between (char q) (char q) p <*> pure q
    qvalue p = ARef <$> reference <|> AChar <$> p

--                        /* any Unicode character, excluding the surrogate blocks, FFFE, and FFFF. */
-- Char	          ::=   	#x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
-- Comment	      ::=   	'<!--' ((Char EXCEPT '-') | ('-' (Char EXCEPT '-')))* '-->'
newtype Comment = Comment String deriving (Eq, Show)
comment :: (TokenParsing m) => m Comment
comment = Comment <$> (string "<!--" *> manyTill (try commentChar) (string "-->"))

commentChar :: CharParsing m => m Char
commentChar = (oneOfSet commentCharSet <?> "Char") <|> (char '-') <* notFollowedBy (char '-')

commentCharSet :: CharSet
commentCharSet = CS.fromList $ "\x9\xa\xd"
  ++ ['\x20'..'\x2c'] ++ ['\x2e'..'\xd7ff'] ++ ['\xe000'..'\xfffd'] ++ ['\x10000'..'\x10ffff']
