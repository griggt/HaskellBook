module DotLang.Util.Combinators
  ( manyThru
  , manyTillSub
  , manyUntil
  , optSepEndBy
  , optSepEndBy1
  ) where

import Control.Applicative (Alternative, (<|>))
import Text.Parser.Combinators (skipOptional)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)

-- my variants of existing combinators

-- like manyTill but does not comsume the input from the `end` parser
manyUntil :: (LookAheadParsing m, Alternative m) => m a -> m end -> m [a]
manyUntil p end = go where go = ([] <$ lookAhead end) <|> ((:) <$> p <*> go)

-- like manyTill but captures the result from the `end` parser
manyThru:: Alternative m => m a -> m end -> (end -> [a]) -> m [a]
manyThru p end f = go where go = (f <$> end) <|> ((:) <$> p <*> go)

-- like manyTill but injects `sub` into the results in place of `end`
manyTillSub :: Alternative m => m a -> m end -> [a] -> m [a]
manyTillSub p end sub = go where go = (sub <$ end) <|> ((:) <$> p <*> go)

---------------------------
-- modified combinators from Text.Parser.Combinators

-- these are like the stock sepEndBy1 and sepEndBy, with the difference being
-- that the separator optionally occurs between instances of `p`

optSepEndBy1 :: Alternative m => m a -> m sep -> m [a]
optSepEndBy1 p sep =
  flip id <$> p
          <*> ((flip (:) <$> ((skipOptional sep) *> optSepEndBy p sep)) <|> pure pure)

optSepEndBy :: Alternative m => m a -> m sep -> m [a]
optSepEndBy p sep = optSepEndBy1 p sep <|> pure []
