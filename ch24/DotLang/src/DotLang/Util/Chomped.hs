{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module DotLang.Util.Chomped where

import Control.Applicative ((<|>), some, Alternative)
import Data.Bifunctor (Bifunctor)
import Data.Char (isSpace)
import Data.Semigroup.Reducer (Reducer(..))
import Text.Parser.Char (satisfy, string, CharParsing)
import Text.Trifecta.Parser (Parser)

-- TODO how useful are Semigroup/Monoid? They concatenate tokens without
--      any interspersed whitespace which makes them unusable.
--       Reducer + foldReduce seems more like what we want.
--       is reducers overkill?

newtype Chomped a b = Chomped (a, b)
  deriving (Eq, Show, Semigroup, Monoid, Functor, Bifunctor, Applicative)

type Chomp a = Chomped String a

instance (Semigroup m, Reducer a m, Reducer b m) => Reducer (Chomped a b) m where
  unit (Chomped (x, y)) = unit y <> unit x

instance (Semigroup a) => Reducer a a where      -- orphan
  unit = id

class CharParsing m => ChompedParsing m where
  chomp :: m a -> m (Chomp a)
  chomp p = do
    x <- p
    s <- chompableSpace <|> pure ""
    return $ Chomped (s, x)

  chompableSpace :: m [Char]
  chompableSpace = some (satisfy isSpace)  -- TODO exclude end-of-line?!?

chompSymbol :: ChompedParsing m => String -> m (Chomp String)
chompSymbol x = chomp (string x)

-- TODO is this used anywhere?
chompSquash :: ChompedParsing m => String -> m (Chomp String)
chompSquash x = chomp ((' ' <$ x) <$ string x)

instance ChompedParsing Parser
