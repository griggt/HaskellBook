{-# LANGUAGE OverloadedStrings #-}

module SemVer where

import Control.Applicative
import Data.List
import Text.Trifecta hiding (release)

data NumberOrString = NOSS String | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord NumberOrString where
  compare (NOSS _) (NOSI _) = GT
  compare (NOSI _) (NOSS _) = LT
  compare (NOSI x) (NOSI y) = compare x y
  compare (NOSS x) (NOSS y) = compare x y

instance Ord SemVer where
  compare (SemVer m n p [] _) (SemVer m' n' p' [] _) = compare [m,n,p] [m',n',p']
  compare (SemVer _ _ _ r  _) (SemVer _  _  _  [] _) = LT
  compare (SemVer _ _ _ [] _) (SemVer _  _  _  r' _) = GT
  compare (SemVer m n p r  _) (SemVer m' n' p' r' _) =
    case compare [m,n,p] [m',n',p'] of
      LT -> LT
      GT -> GT
      EQ -> compare r r'

-- Prettify a SemVer by turning it back into a formatted string
prettyNOS :: NumberOrString -> String
prettyNOS (NOSS s) = s
prettyNOS (NOSI x) = show x

prettySemVer :: SemVer -> String
prettySemVer (SemVer m n p r b) = core ++ (optional r '-') ++ (optional b '+')
  where
    core = show m ++ "." ++ show n ++ "." ++ show p
    optional x c = if null x then "" else c : (intercalate "." $ map prettyNOS x)

--------------------------------------------------------------------------------------
-- Parsing!

parseSemVer :: Parser SemVer
parseSemVer = do
  maj <- major
  char '.'
  min <- minor
  char '.'
  patch <- patch

  rel <- option [] release
  meta <- option [] metadata

  return $ SemVer maj min patch rel meta

major :: Parser Major
major = decimal

minor :: Parser Minor
minor = decimal

patch :: Parser Patch
patch = decimal

release :: Parser Release
release = do
  char '-'
  dotSep1 releaseId

metadata :: Parser Metadata
metadata = do
  char '+'
  dotSep1 buildId

----

releaseId :: Parser NumberOrString
releaseId = (NOSS <$> alphaId) <|> (NOSI <$> decimal)

buildId :: Parser NumberOrString
buildId = (NOSS <$> alphaId) <|> (NOSI <$> decimal)

alphaId :: Parser String
alphaId = try alphaId1
      <|> try alphaId3
      <|> alphaId2

alphaId1 :: Parser String
alphaId1 = do
  a <- nondigit
  b <- some idchar
  return ([a] ++ b)

alphaId2 :: Parser String
alphaId2 = count 1 nondigit

alphaId3 :: Parser String
alphaId3 = do
  a <- some digit
  b <- nondigit
  c <- many idchar
  return (a ++ [b] ++ c)

nondigit :: Parser Char
nondigit = letter <|> (char '-')

idchar :: Parser Char
idchar = digit <|> nondigit

dotSep1 :: Parser a -> Parser [a]
dotSep1 p = sepBy1 p (char '.')
