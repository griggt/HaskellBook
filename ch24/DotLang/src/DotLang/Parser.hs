{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}

module DotLang.Parser
  ( Graph(..)
  , GraphType(..)
  , Statement(..)
  , AttributeType(..)
  , Attribute(..)
  , AttrList
  , Assignment(..)
  , Edge(..)
  , EdgeRhs(..)
  , Endpoint(..)
  , Node(..)
  , NodeId(..)
  , Port(..)
  , Subgraph(..)
  , CompassPt(..)
  , EdgeOp(..)
  , Identifier(..)
  , parseGraphFile
  , parseGraphString
  , DotToken(..)  -- TODO TEMP
  , dotFile  -- TODO TEMP
  , dotLine  -- TODO TEMP
  , dotToken -- TODO TEMP
  , dotFileLine -- TODO TEMP
  , dotKeyword  -- TEMP
  , dotQuoted   -- TEMP
  , dotIdentifier -- TEMP
  , dotOperator   -- TEMP
  , dotSeparator -- TEMP
  , dotGrouping  -- TEMP
  , spanComment  -- TEMP
  , endComment   -- TEMP
  , dotHtml
  )
  where

import Control.Applicative ((<|>), liftA2, liftA3)
import Control.Monad.IO.Class (MonadIO)
import Data.Char (isDigit, isLetter)
import Data.Semigroup.Reducer (Reducer(..), foldReduce)
import Data.String (IsString, fromString)

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token hiding (stringLiteral)
import Text.Parser.Token.Highlight

import Text.Trifecta (parseFromFileEx, parseString)
import Text.Trifecta.Result (Result(..))

import qualified DotLang.Parser.Xml as XML
import DotLang.Util.Char
import DotLang.Util.Chomped
import DotLang.Util.Combinators

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

(<<$) :: (Functor f1, Functor f2) => b -> f1 (f2 a) -> f1 (f2 b)
(<<$) = fmap . fmap . const

------------------------------------------------------------------------------------------------
-- Entry points

-- TODO WONTFIX parseFromFile uses parseFromFileEx which assumes UTF-8 encoding
--              Other parts of Trifecta have baked-in reliance on UTF-8 as well.

parseGraphFile :: (MonadIO m) => FilePath -> m (Result Graph)
parseGraphFile fp = do     -- outer do is for IO
  fr <- parseFromFileEx dotFile fp
  return $ do foldReduce <$> fr >>= parseString graph mempty  -- inner do is for Result

parseGraphString :: String -> Result Graph
parseGraphString s = parse graph =<< foldReduce <$> parse dotFile s
  where parse p = parseString p mempty

------------------------------------------------------------------------------------------------
-- EXPERIMENTAL lexing / pre-processing for comment handling

-- everywhere in the first pass, keep the layout of the output the same as
--      the input. this means that for all comments and whitespace that gets
--      chomped, the equivalent whitespace (' ' or '\n' needs to be re-injected)
--       This is to keep row/col numbers for error messages in pass 2 accurate.

-- TODO is is possible to adapt Trifecta to take a stream of tokens as input,
--    rather than a string or ByteString?  I suppose so if I could create a
--    way to reduce the token stream to a Rope (parser input), or build a rope
--    out of a token stream.  The predefined Reducer instances are in the Rope.hs file.

data DotToken =
    TokEmpty
  | TokKeyword String
  | TokIdentifier String
  | TokQuoted String
  | TokOperator String
  | TokGrouping Char
  | TokSeparator Char
  | TokComment String
  | TokHtml [String]
  deriving (Eq, Show)

dtString :: DotToken -> String
dtString (TokEmpty) = ""
dtString (TokKeyword x) = x
dtString (TokIdentifier x) = x
dtString (TokQuoted x) = '"' : x ++ ['"']
dtString (TokOperator x) = x
dtString (TokComment x) = ' ' <$ x     -- replace comment text with whitespace
dtString (TokGrouping x) = pure x
dtString (TokSeparator x) = pure x
dtString (TokHtml xs) = concat xs

instance Reducer DotToken String where
  unit = dtString

-- TODO FIXME this is failing on files that do not have a final newline
--        c.f. siblings.dot
dotFile :: (LookAheadParsing m, ChompedParsing m) => m [Chomp DotToken]
dotFile = concat <$> some dotFileLine

dotFileLine :: (LookAheadParsing m, ChompedParsing m) => m [Chomp DotToken]
dotFileLine = dotLineComment <|> dotLine

dotLine :: (LookAheadParsing m, ChompedParsing m) => m [Chomp DotToken]
dotLine = try mixedContent <|> try someContent <|> try endCommentO <|> emptyLine
  where
    emptyLine    = eol <|> trailing (some hspace)
    endCommentO  = c3 <$> optionalSpace <*> endingComment             <*> maybeEol
    someContent  = c3 <$> optionalSpace <*> content                   <*> maybeEol
    mixedContent = c4 <$> optionalSpace <*> content <*> endingComment <*> maybeEol

    -- Components
    optionalSpace = maybe [] pure <$> (optional (const TokEmpty <<$>> chompSomeSpace))
    content       = some (dotToken <|> (TokComment <<$>> spanComment))
    endingComment = pure <$> (TokComment <<$>> endComment)
    eol           = pure <$> (const TokEmpty) <<$>> chompSpaceWith endOfLine'
    maybeEol      = pure <$> (const TokEmpty) <<$>> chompSpaceWith (option [] endOfLine')
    trailing s    = pure <$> ((const TokEmpty) <<$>> (chompSpaceWith $ liftA2 (<>) (s) (option [] endOfLine')))

    -- TODO do we really need to be concerned with potential hspace before newline?
    --       shouldn't it have been chomped by A, B, or C?  (except for emptyLine case)

    --      dotToken
    --        dotKeyword    Chomps?  yes   Tested?  yes
    --        dotQuoted     Chomps?  yes   Tested?  yes
    --        dotIdentifier Chomps?  yes   Tested?  yes
    --        dotHtml       Chomps?  yes   Tested?  yes
    --        dotOperator   Chomps?  yes   Tested?  yes
    --        dotSeparator  Chomps?  yes   Tested?  yes
    --        dotGrouping   Chomps?  yes   Tested?  yes
    --      spanComment     Chomps?  yes   Tested?  yes
    --      endingComment   Chomps?  yes   Tested?  captured as par of comment text

    -- TODO should the original definition of endOfLine include EOF as it does?
    endOfLine' :: CharParsing m => m [Char]
    endOfLine' = string "\r\n" <|> (pure <$> char '\n')

    c3 x y z = x <> y <> z
    c4 w x y z = w <> x <> y <> z

    -- Maybe we need several productions.
    ------ empty line (only whitespace)                                                     D
    ------ required token or span comment (with optional leading/trailing space)  [A]  B   [D]
    ------ required end comment (with optional leading trailing / space)          [A]    C [D]
    ------ both tokens/end comment with optional leading trailing/space           [A]  B C [D]
    ----------- is the assumption here that D is (some hspace) + at most one (optional) endOfLine?

dotLineComment :: (LookAheadParsing m, ChompedParsing m) => m [Chomp DotToken]
dotLineComment = pure <$> (TokComment <<$>> lineComment)

-- TODO order the alternatives appropriately
dotToken :: ChompedParsing m => m (Chomp DotToken)
dotToken = try (TokKeyword     <<$>> dotKeyword)
            <|> TokQuoted      <<$>> dotQuoted
            <|> TokIdentifier  <<$>> dotIdentifier
            <|> TokHtml        <<$>> dotHtml
            <|> TokOperator    <<$>> dotOperator
            <|> TokSeparator   <<$>> dotSeparator
            <|> TokGrouping    <<$>> dotGrouping

dotKeyword :: ChompedParsing m => m (Chomp String)
dotKeyword = chompKeyword "digraph" <|> chompKeyword "edge" <|> chompKeyword "graph"
          <|> chompKeyword "node" <|> try (chompKeyword "strict") <|> chompKeyword "subgraph"

dotQuoted :: ChompedParsing m => m (Chomp String)
dotQuoted = chomp quotedString   -- we replace the lost quotes in dtString

dotSeparator :: ChompedParsing m => m (Chomp Char)
dotSeparator = chomp (char ',') <|> chomp (char ';') <|> chomp (char ':')

dotGrouping :: ChompedParsing m => m (Chomp Char)
dotGrouping = chomp $ oneOf "{}[]" <?> "{, }, [, ]"

dotOperator :: ChompedParsing m => m (Chomp String)
dotOperator = (chompSymbol "=") <|> (chompSymbol "--") <|> (chompSymbol "->")

dotHtml :: ChompedParsing m => m (Chomp [String])
dotHtml = (\a b c -> a <> b <> c) <$> mark '<' <*> body <*> mark '>'
  where
    mark c = (pure . pure) <<$>> chomp (char c)
    body = chomp XML.lexElement
    -- TODO don't capture the < and >, just add back later in dtString
    --      like we do for quoted strings

-- TODO FIXME this heuristic is too simple for numeric ids, due to possible
--            unary negation and overlap of the '-' symbol w/edge ops
dotIdentifier :: ChompedParsing m => m (Chomp String)
dotIdentifier = chomp $ some (satisfy isDotIdChar) <?> "identifier"

isDotIdChar :: Char -> Bool   -- skipping '\128' - '\255'
isDotIdChar c = isLetter c || isDigit c || c == '_' || c == '.' -- || c == '-'

-- for these comments that extend to the end-of-line, we really want the
--   newline to appear in the left side (whitespace) of the Chomped, not the
--   right (data) side, because the right side will later be discarded

comment :: ChompedParsing m => m String -> m String -> m (Chomp String)
comment beg end = liftA2 (<>) (pure <$> beg) (chomp $ manyThru anyChar (try end) id)

comment' :: (LookAheadParsing m, ChompedParsing m) => m String -> m String -> m (Chomp String)
comment' beg end = liftA2 (<>) (pure <$> beg) (chomp $ manyUntil anyChar (try end))

lineComment :: (LookAheadParsing m, ChompedParsing m) => m (Chomp String)
lineComment = comment' (string "#") endOfLine

endComment :: (LookAheadParsing m, ChompedParsing m) => m (Chomp String)
endComment = comment' (string "//") endOfLine

spanComment :: ChompedParsing m => m (Chomp String)
spanComment = comment (string "/*") (string "*/")

-- TODO FIXME we need to treat any use of '\' not followed by '"' or '\n' as
--   not an escape prefix, but as a literal
-- adapted from Text.Parser.Token stringLiteral in parsers
quotedString :: CharParsing m => m String
quotedString = literal where
  literal = foldr (maybe id (:)) ""
    <$> between (char '"') (char '"' <?> "end of string") (many stringChar)
    <?> "string"

  stringChar = Just <$> stringLetter <|> try falseEscape <|> stringEscape <?> "string character"
  stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))
  falseEscape = Just <$> char '\\' <* notFollowedBy (escapeCode <|> space)

  stringEscape = char '\\' *> esc
    where esc = Nothing <$ escapeGap
            <|> Just <$> escapeCode

  -- TODO is the escape "gap" different in Haskell than in C?
  escapeGap = skipSome space *> (char '\\' <?> "end of string gap")
  escapeCode = char '"'

------------------------------------------------------------------------------------------------
-- Miscellaneous

chompKeyword :: ChompedParsing m => String -> m (Chomp String)
chompKeyword x = chomp (ignoreCase x <* notFollowedBy alphaNumericChar) <?> show x
------------------------------------------------------------------------------------------------

{-  TODO Pass 1
        * preserve whitspace layout so error messages from next
          pass will have appropriate line/column indications!

        * handle C++ style comments

        * handle preprocessor lines (beginning w/#)

        * handle line continuation for "" strings (backslash)
          [incorporate into string literal parser?]

        * handle quoted string concatenation (+)
          [add a production rule around string literals?]
 -}
-- TODO Pass 2


-----------------------------------------------------------------------------------------------
-- DOT grammar
-----------------------------------------------------------------------------------------------

-- GRAPH:
--      ["strict"] <graph-type> [<identifier>] '{' <statement-list> '}'

data Graph =
  Graph {
    gType :: GraphType
  , gName :: Maybe Identifier
  , gIsStrict :: Bool
  , gStatements :: [Statement]
  }
  deriving (Eq, Show)

graph :: TokenParsing m => m Graph
graph = do
  _   <- whiteSpace
  iss <- option False (True <$ keyword "strict")
  gt  <- graphType
  gid <- optional identifier
  gst <- braces statementList
  return $ Graph gt gid iss gst

-- GRAPH-TYPE:
--      "graph" | "digraph"

data GraphType =
    UndirectedGraph
  | DirectedGraph
  deriving (Eq, Show)

graphType :: TokenParsing m => m GraphType
graphType = (UndirectedGraph <$ keyword "graph") <|> (DirectedGraph <$ keyword "digraph")

-- STATEMENT-LIST:
--      [<statement> [';'] <statement-list>]

statementList :: TokenParsing m => m [Statement]
statementList = optSepEndBy statement semi

-- STATEMENT:
--      <node-statement>
--    | <edge-statement>
--    | <attribute-statement>
--    | <identifier> '=' <identifier>
--    | <subgraph>

data Statement =
    NodeStatement Node
  | EdgeStatement Edge
  | AttributeStatement Attribute
  | AssignmentStatement Assignment
  | SubgraphStatement Subgraph
  deriving (Eq, Show)

-- TODO do I need all the trys or just between Node/Edge stmts?
statement :: TokenParsing m => m Statement
statement = try (SubgraphStatement <$> subgraph)
        <|> try (AttributeStatement <$> attributeStatement)
        <|> try (AssignmentStatement <$> assignment)
        <|> try (EdgeStatement <$> edgeStatement)
        <|> (NodeStatement <$> nodeStatement)

-- ATTRIBUTE-STATEMENT:
--      ("graph" | "node" | "edge") <attribute-list>

data AttributeType =
    GraphAttribute
  | NodeAttribute
  | EdgeAttribute
  deriving (Eq, Show)

attributeType :: TokenParsing m => m AttributeType
attributeType = (GraphAttribute <$ keyword "graph")
            <|> (NodeAttribute  <$ keyword "node")
            <|> (EdgeAttribute  <$ keyword "edge")

data Attribute = Attribute AttributeType [[Assignment]] deriving (Eq, Show)

attributeStatement :: TokenParsing m => m Attribute
attributeStatement = Attribute <$> attributeType <*> attributeList <?> "attribute statement"

type AttrList = [[Assignment]]

-- ATTRIBUTE-LIST:
--      '[' [<assign-list>] ']' [<attribute-list>]
attributeList :: TokenParsing m => m AttrList
attributeList = some (brackets (option [] assignList)) <?> "attribute list"

-- ASSIGN-LIST:
--      <assignment> [(';' | ',')] [<assign-list>]

assignList :: TokenParsing m => m [Assignment]
assignList = optSepEndBy1 assignment (symbolic ';' <|> symbolic ',')

-- ASSIGNMENT:
--      <identifier> '=' <identifier>

data Assignment = Assignment Identifier Identifier deriving (Eq, Show)

assignment :: TokenParsing m => m Assignment
assignment = do
  lhs <- identifier
  _   <- symbolic '='
  rhs <- identifier
  return (Assignment lhs rhs) -- <?> "assignment"

-- EDGE-STATEMENT:
--      <endpoint> <edge-rhs> [<attribute-list>]

data Edge = Edge Endpoint [EdgeRhs] AttrList
  deriving (Eq, Show)

edgeStatement :: TokenParsing m => m Edge
edgeStatement = Edge <$> endpoint <*> edgeRhs <*> option [] attributeList  -- TODO [] or [[]]? does it matter?

-- EDGE-RHS:
--      <edgeop> <endpoint> [<edge-rhs>]

data EdgeRhs = EdgeRhs EdgeOp Endpoint deriving (Eq, Show)

edgeRhs :: TokenParsing m => m [EdgeRhs]
edgeRhs = some (EdgeRhs <$> edgeOp <*> endpoint) <?> "edge RHS"

-- ENDPOINT:
--      <node-id> | <subgraph>

data Endpoint =
    NodeEndpoint NodeId
  | SubgraphEndpoint Subgraph
  deriving (Eq, Show)

endpoint :: TokenParsing m => m Endpoint
endpoint = (NodeEndpoint <$> nodeId) <|> (SubgraphEndpoint <$> subgraph) <?> "endpoint"

-- NODE-STATEMENT:
--      <node-id> [<attribute-list>]

data Node = Node NodeId AttrList deriving (Eq, Show)

nodeStatement :: TokenParsing m => m Node
nodeStatement = Node <$> nodeId <*> option [] attributeList  -- TODO [] or [[]]? does it matter?

-- NODE-ID:
--      <identifier> [<port>]

data NodeId = NodeId Identifier (Maybe Port) deriving (Eq, Show)

nodeId :: TokenParsing m => m NodeId
nodeId = NodeId <$> identifier <*> optional port <?> "node id"

-- PORT:
--      ':' (<identified-port> | <compass-pt)

-- IDENTIFIED-PORT:
--    <identifier> [':' <compass-pt>]

data Port = Port (Maybe Identifier) (Maybe CompassPt) deriving (Eq, Show)

port :: TokenParsing m => m Port
port = colon *> (identifiedPort
            <|> (Port <$> (pure Nothing) <*> (Just <$> compassPt)))
            <?> "port"

identifiedPort :: TokenParsing m => m Port
identifiedPort = do
  pid <- identifier
  cpt <- optional (colon *> compassPt)
  return $ Port (Just pid) cpt

-- SUBGRAPH:
--      <anonymous-subgraph>
--    | <named-subgraph>

data Subgraph =
  Subgraph {
    sgName :: Maybe Identifier
  , sgStatements :: [Statement]
  }
  deriving (Eq, Show)

subgraph :: TokenParsing m => m Subgraph
subgraph = anonymousSubgraph <|> namedSubgraph <?> "subgraph"

-- ANONYMOUS-SUBGRAPH:
--      ["subgraph"] '{' <statement-list> '}'

anonymousSubgraph :: TokenParsing m => m Subgraph
anonymousSubgraph = do
  _   <- skipOptional $ keyword "subgraph"
  sid <- optional identifier
  sst <- braces statementList
  return (Subgraph sid sst) -- <?> "anonymous subgraph"

-- NAMED-SUBGRAPH:
--      "subgraph" <identifier> '{' <statement-list> '}'

namedSubgraph :: TokenParsing m => m Subgraph
namedSubgraph = do
  _   <- keyword "subgraph"
  sid <- identifier
  sst <- braces statementList
  return (Subgraph (Just sid) sst) -- <?> "named subgraph"


-- COMPASS-PT:
--      ("n" | "ne" | "e" | "se" | "s" | "sw" | "w" | "nw" | "c" | _)
newtype CompassPt = CompassPt String deriving (Eq, Show)

compassPt :: TokenParsing m => m CompassPt
compassPt = CompassPt <$>
              (symbol "n" <|> symbol "ne" <|> symbol "se" <|> symbol "s"
              <|> symbol "sw" <|> symbol "w" <|> symbol "nw" <|> symbol "c")
              <?> "compass point"
          -- TODO are we supposed to accept any string here?

-- EDGEOP:
--      "->"    in directed graphs
--      "--"    in undirected graphs

data EdgeOp =
    EdgeOpDirected
  | EdgeOpUndirected
  deriving (Eq, Show)

edgeOp :: TokenParsing m => m EdgeOp
edgeOp = (EdgeOpDirected <$ symbol "->")
     <|> (EdgeOpUndirected <$ symbol "--")
     <?> "edge op"

-- IDENTIFIER:
--      <alphanumeric-id>
--    | <numeric-id>
--    | <quoted-id>
--    | <html-string>       (yow!)

-- TODO look into using Identifier tokens / style (ident, reserve, etc.)

data Identifier =
    AlphaNumId String
  | NumericId (Either Integer Double)
  | QuotedId String
  | HtmlId XML.Element
  deriving (Eq, Show)

identifier :: TokenParsing m => m Identifier
identifier = alphaNumericId <|> numericId <|> quotedId <|> htmlString <?> "identifier"

-- ALPHANUMERIC-ID:
--      <nondigit>
--    | <nondigit> <alphanumeric>
alphaNumericId :: TokenParsing m => m Identifier
alphaNumericId = AlphaNumId <$> token ((:) <$> nondigit <*> many alphaNumericChar)

-- NUMERIC-ID:
--      ['-'] '.' <digit>+
--    | ['-'] <digit>+ ('.' <digit>*)?
-- TODO I don't think we care about the numeric value, just the string rep?
--      Or is this not so?  This is used on the RHS of attribute assignment.
-- TODO FIXME integerOrDouble parser doesn't handle case of leading decimal point
numericId :: TokenParsing m => m Identifier
numericId = NumericId <$> integerOrDouble

-- QUOTED-ID:
--      '"' ...anything...  '"'     -- only escape char is \"
quotedId :: TokenParsing m => m Identifier
quotedId = QuotedId <$> stringLiteral

stringLiteral :: (TokenParsing m, IsString s) => m s
stringLiteral = fromString <$> token (highlight StringLiteral quotedString)

-- HTML-STRING:
--    '<' [any legal XML string] '>'
htmlString :: TokenParsing m => m Identifier
htmlString = HtmlId <$> angles XML.parseElement

-- ALPHANUMERIC:
--      <alphanumeric-char>
--    | <alphanumeric-char> <alphanumeric>
alphanumeric :: CharParsing m => m [Char]
alphanumeric = (:) <$> alphaNumericChar <*> many alphaNumericChar

-- ALPHANUMERIC-CHAR:      -- TODO this name is confusing, because alphaNum is pre-existing, and does not include '_'
--    <nondigit> | <digit>
alphaNumericChar :: CharParsing m => m Char
alphaNumericChar = nondigit <|> digit

-- NONDIGIT:
--    one from character set [_a-zA-Z\200-\377]

nondigit :: CharParsing m => m Char
nondigit = letter <|> (char '_') -- <|> oneOf ['\128'..'\255']

-- DIGIT:
--    one from character set [0-9]

-- We use digit from Text.Parser.Char

----------------------------------------------------------------------------------------------------
-- Utilities

keyword :: TokenParsing m => String -> m String
keyword name = token (highlight ReservedIdentifier kw) <?> (show name)
  where kw = ignoreCase name <* notFollowedBy alphaNumericChar

-------------------------------------------------------------------------------------------------
-- NOTES:
--   keywords { node, edge, graph, digraph, subgraph, strict} are case-insensitive
--   compass point values are NOT keywords, may be used as identifiers
--   C++ style comments are supported (//) and (/* */)
--   Lines beginning with '#' are discarded
--   Lines ending with a backslash indicate continuation (like in C) for "" strings
--   Double quoted strings may be concatenated using a '+' operator
--
--   <quoted-id> and <html-string> are scanned as units, so embedded comments are
--   treated as part of the strings.
--
--   when <html-string> is used as a label attribute, it is interpreted specially
--   and must follow the syntax for HTML-like labels

-- TODO
-- what is the last underscore in <compass-pt>??
