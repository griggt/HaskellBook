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
import Text.Parser.Char (satisfy, space, string, CharParsing)
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
  chompableSpace = some hspace

hspace :: CharParsing m => m Char
hspace = satisfy isHspace

isHspace :: Char -> Bool
isHspace c = (c == ' ') || (c == '\t')

-- TODO refactor out the pattern commmon to all chomp**Space functions
--       (and chomp function itself)

chompSpace :: ChompedParsing m => m (Chomp String)
chompSpace = do
  s <- chompableSpace <|> pure ""
  return $ Chomped (s, "")

chompSomeSpace :: ChompedParsing m => m (Chomp String)
chompSomeSpace = do
  s <- some space
  return $ Chomped (s, "")

chompAllSpace :: ChompedParsing m => m (Chomp String)
chompAllSpace = do
  s <- (some space) <|> pure ""
  return $ Chomped (s, "")

chompSymbol :: ChompedParsing m => String -> m (Chomp String)
chompSymbol x = chomp (string x)

-- TODO is this used anywhere?
chompSquash :: ChompedParsing m => String -> m (Chomp String)
chompSquash x = chomp ((' ' <$ x) <$ string x)

instance ChompedParsing Parser
