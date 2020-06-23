{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ApplicativeDo #-}

module DotLang.Util.Chomped
  ( Chomped(..)
  , Chomp
  , ChompedParsing(..)
  , hspace
  , chompAllSpace
  , chompSomeSpace
  , chompSpace
  , chompSpaceWith
  , chompSymbol
  , chompSquash
  ) where

import Control.Applicative ((<|>), some, Alternative)
import Data.Bifunctor (Bifunctor)
import Data.Char (isSpace)
import Data.Semigroup.Reducer (Reducer(..))
import Text.Parser.Char (satisfy, space, string, CharParsing)
import Text.Trifecta as Trifecta

newtype Chomped a b = Chomped (a, b)
  deriving (Eq, Show, Semigroup, Monoid, Functor, Bifunctor, Applicative)

-- shorthand for most common use case
type Chomp a = Chomped String a

-- reduce a Chomped to a Semigroup
instance (Semigroup m, Reducer a m, Reducer b m) => Reducer (Chomped a b) m where
  unit (Chomped (x, y)) = unit y <> unit x

class CharParsing m => ChompedParsing m where
  chomp :: m a -> m (Chomp a)
  chomp = chompWith chompableSpace

  -- NB chompAll can chomp multiple consecutive newlines and any succeding whitespace
  chompAll :: m a -> m (Chomp a)
  chompAll = chompWith (some space)

  chompWith :: (Monoid s) => m s -> m a -> m (Chomped s a)
  chompWith sp p = do
    x <- p
    s <- sp <|> pure mempty
    return $ Chomped (s, x)

  chompableSpace :: m [Char]
  chompableSpace = some hspace

hspace :: CharParsing m => m Char
hspace = satisfy isHspace

isHspace :: Char -> Bool
isHspace c = (c == ' ') || (c == '\t')

chompSpaceWith ::ChompedParsing m => m s -> m (Chomped s ())
chompSpaceWith sp = do
  s <- sp
  return $ Chomped (s, ())

chompSpace :: ChompedParsing m => m (Chomp ())
chompSpace = chompSpaceWith $ chompableSpace <|> pure ""

chompSomeSpace :: ChompedParsing m => m (Chomp ())
chompSomeSpace = chompSpaceWith $ some hspace

chompAllSpace :: ChompedParsing m => m (Chomp ())
chompAllSpace = chompSpaceWith $ (some space) <|> pure ""

chompSymbol :: ChompedParsing m => String -> m (Chomp String)
chompSymbol x = chomp (string x)

-- TODO is this used anywhere?
chompSquash :: ChompedParsing m => String -> m (Chomp String)
chompSquash x = chomp ((' ' <$ x) <$ string x)

instance ChompedParsing Trifecta.Parser
