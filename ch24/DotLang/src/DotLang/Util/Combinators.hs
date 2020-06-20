module DotLang.Util.Combinators where

import Control.Applicative (Alternative, (<|>))
import Text.Parser.Combinators (skipOptional)

-- my variant of an existing combinator
manyTillSub :: Alternative m => m a -> m end -> [a] -> m [a]
manyTillSub p end sub = go where go = (sub <$ end) <|> ((:) <$> p <*> go)

-- my variant of an existing combinator
manyThru:: Alternative m => m a -> m end -> (end -> [a]) -> m [a]
manyThru p end f = go where go = (f <$> end) <|> ((:) <$> p <*> go)

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
