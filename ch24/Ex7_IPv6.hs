module IPv6Address where

import Control.Applicative (liftA2, Alternative, (<|>))
import Data.Bits ((.|.), (.&.), shiftL, shiftR, finiteBitSize, Bits, FiniteBits)
import Data.Char (digitToInt, intToDigit)
import Data.List (intercalate, unfoldr)
import Data.Foldable (foldl')
import Data.Word (Word16, Word32, Word64)
import Numeric (showHex)
import Test.QuickCheck (quickCheck, withMaxSuccess, Arbitrary(..))
import Text.Trifecta

import Ex6_IPv4 (IPAddress(..), showIPAddress, ipaddr')

data IPAddress6 = IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)

data Block =
    Hextet Word16
  | DoubleColon
  | IP4Addr IPAddress
  deriving (Eq, Show)

-- TODO FIXME Is this a valid string to parse "fe80::192.0.2.128"  ??
--   It fails using ipv4TransitionalAddress (but works if I add a third colon)

-- TODO add error contexts, esp. when we call try; use <?>

---------------------------------------------------------------
-- A potential strategy, somewhat chaotic:
--   Read out a bunch of hexadecimal blocks separated by colons
--   Some ground rules:
--     - no more than four hex character per block
--     - no more than 8 blocks total (or 7 colon delimiters)
--     - no more than one instance of two adjacent delimiters (::)
--     - no whitespace
--     - handle IPv4 format last 32 bits as special case
--
-- Maybe a better idea, write as a CFG in BNF:

----------------------------------------------------------------
--   ** BNF GRAMMAR **
----------------------------------------------------------------

--   IPV6-ADDRESS ::=
--      <canonical-address>
--    | <abbreviated-address>
--    | <ipv4-transitional-address>

-- TODO move this IP4 transitional stuff to the bottom of the top-level parsers section

--   IPV4-TRANSITIONAL-ADDRESS ::=
--      <canonical-ipv4-transitional-address>
--    | <abbreviated-ipv4-transitional-address>

ipv4TransitionalAddress :: Parser IPAddress6
ipv4TransitionalAddress = try canonicalIPv4Transitional <|> abbreviatedIPv4Transitional

--   CANONICAL-IPV4-TRANSITIONAL-ADDRESS ::=
--      <canonical-6-hextet> <ipv4-address>

canonicalIPv4Transitional :: Parser IPAddress6
canonicalIPv4Transitional = do
  lhs <- canonical6Hextet
  rhs <- ipaddr'
  return $ blocksToAddress $ (Hextet <$> lhs) ++ [IP4Addr rhs]

--   ABBREVIATED-IPV4-TRANSITIONAL-ADDRESS ::=
--      <abbreviated-6-hextet> <ipv4-address>

abbreviatedIPv4Transitional :: Parser IPAddress6
abbreviatedIPv4Transitional = do
  lhs <- abbreviated6Hextet
  rhs <- ipaddr'
  return $ blocksToAddress (lhs ++ [IP4Addr rhs])

--   CANONICAL-6-HEXTET ::=
--      <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':'

canonical6Hextet :: Parser [Word16]
canonical6Hextet = sepByN 6 hextet (char ':') <* (char ':')  -- TODO create an endByN also?

--   ABBREVIATED-6-HEXTET ::=
--      <up-to-four-blocks>   "::"
--    | <up-to-three-blocks>  "::" hextet ':'
--    | <up-to-two-blocks>    "::" hextet ':' hextet ':'
--    | <up-to-one-block>     "::" hextet ':' hextet ':' hextet ':'
--    | <zero-blocks>         "::" hextet ':' hextet ':' hextet ':' hextet ':'

abbreviated6Hextet :: Parser [Block]
abbreviated6Hextet = choice (reverse abbrevs)   -- TODO ugggh, reverse
  where abbrevs = [try $ abbrevBlocks n <* (char ':') | n <- [0..4]]

--   IPV4-ADDRESS ::=
--      <octet> '.' <octet> '.' <octet> '.' <octet>

--   OCTET ::=
--      <decimal>       -- in range [0..255]

--   NB For parsing the IPv4 address portion we use `ipaddr'` imported from Ex6_IPv4

ipv6Address :: Parser IPAddress6
ipv6Address = try canonicalAddress <|> abbreviatedAddress   -- TODO IPv4 compatibility address

--   CANONICAL-ADDRESS ::=
--      <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet>

-- for notational convenience
(<<*>>) = fmap . fmap

canonicalAddress :: Parser IPAddress6
canonicalAddress = blocksToAddress <$> Hextet <<*>> sepByN 8 hextet (char ':')

-- canonicalAddress = do
--   half1 <- sepByN 4 hextet (char ':')
--   char ':'
--   half2 <- sepByN 4 hextet (char ':')
--   return $ IPAddress6 (combineHextets half1) (combineHextets half2)
--   where combineHextets [a, b, c, d] = shiftL (fromIntegral a) 48
--                                   .|. shiftL (fromIntegral b) 32
--                                   .|. shiftL (fromIntegral c) 16
--                                   .|.        (fromIntegral d)

--   Allowed abbreviations:
--      omit leading zeros in any hextet
--      omit a single (multi-hextet) all-zero run of hextets
--
--   We handle leading zeros of a particular hextext in the `hextet` rule.
--   N.B. RFC 5952 requires that a double colon _not_ be used to denote an omitted _single_ section of zeros.
--
--   ABBREVIATED-ADDRESS ::=    // (28 == 8 choose 6 alternatives)
--      <hextet> : <hextet> : <hextet> : <hextet> : <hextet> : <hextet> ::
--    | <hextet> : <hextet> : <hextet> : <hextet> : <hextet> :: <hextet>
--    | <hextet> : <hextet> : <hextet> : <hextet> : <hextet> ::
--    | <hextet> : <hextet> : <hextet> : <hextet> :: <hextet> : <hextet>
--    | <hextet> : <hextet> : <hextet> : <hextet> :: <hextet>
--    | <hextet> : <hextet> : <hextet> : <hextet> ::
--    | <hextet> : <hextet> : <hextet> :: <hextet> : <hextet> : <hextet>
--    | <hextet> : <hextet> : <hextet> :: <hextet> : <hextet>
--    | <hextet> : <hextet> : <hextet> :: <hextet>
--    | <hextet> : <hextet> : <hextet> ::
--    | <hextet> : <hextet> :: <hextet> : <hextet> : <hextet> : <hextet>
--    | <hextet> : <hextet> :: <hextet> : <hextet> : <hextet>
--    | <hextet> : <hextet> :: <hextet> : <hextet>
--    | <hextet> : <hextet> :: <hextet>
--    | <hextet> : <hextet> ::
--    | <hextet> :: <hextet> : <hextet> : <hextet> : <hextet> : <hextet>
--    | <hextet> :: <hextet> : <hextet> : <hextet> : <hextet>
--    | <hextet> :: <hextet> : <hextet> : <hextet>
--    | <hextet> :: <hextet> : <hextet>
--    | <hextet> :: <hextet>
--    | <hextet> ::
--    | :: <hextet> : <hextet> : <hextet> : <hextet> : <hextet> : <hextet>
--    | :: <hextet> : <hextet> : <hextet> : <hextet> : <hextet>
--    | :: <hextet> : <hextet> : <hextet> : <hextet>
--    | :: <hextet> : <hextet> : <hextet>
--    | :: <hextet> : <hextet>
--    | :: <hextet>
--    | ::

--   ABBREVIATED-ADDRESS ::=     // [concise encoding]
-- 7    <up-to-six-blocks>   "::"
-- 6  | <up-to-five-blocks>  "::" hextet
-- 5  | <up-to-four-blocks>  "::" hextet ':' hextet
-- 4  | <up-to-three-blocks> "::" hextet ':' hextet ':' hextet
-- 3  | <up-to-two-blocks>   "::" hextet ':' hextet ':' hextet ':' hextet
-- 2  | <up-to-one-block>    "::" hextet ':' hextet ':' hextet ':' hextet ':' hextet
-- 1  | <zero-blocks>        "::" hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet

--    TODO can I perform less backtracking if I put the variable length
--         block runs on the right hand side of the '::' ?

abbreviatedAddress :: Parser IPAddress6
abbreviatedAddress = choice (reverse abbrevs)   -- TODO ugggh, reverse
  -- argument `n' is number of fixed blocks to right of double colon
  where abbrevs = [try $ blocksToAddress <$> abbrevBlocks n | n <- [0..6]]

-- TODO can I write this cleaner, maybe without do-notation?
abbrevBlocks :: Int -> Parser [Block]
abbrevBlocks n = do
  lhs <- upToNBlocks $ 6 - n
  dc <- doubleColon
  rhs <- sepByN n hextet (char ':')
  return $ (Hextet <$> lhs) ++ (dc : (Hextet <$> rhs))

--   UP-TO-SIX-BLOCKS ::=
--      <hextet> ':' <up-to-five-blocks>
--    | <up-to-five-blocks>

--   UP-TO-FIVE-BLOCKS ::=
--      <hextet> ':' <up-to-four-blocks>
--    | <up-to-four-blocks>

--   UP-TO-FOUR-BLOCKS ::=
--      <hextet> ':' <up-to-three-blocks>
--    | <up-to-three-blocks>

--   UP-TO-THREE-BLOCKS ::=
--      <hextet> ':' <up-to-two-blocks>
--    | <up-to-two-blocks"

--   UP-TO-TWO-BLOCK ::=
--      <hextet> ':' <up-to-one-block>
--    | <up-to-one-block>

--   UP-TO-ONE-BLOCK ::=
--      <hextet>
--    | <zero-blocks>

--   ZERO-BLOCKS ::=
--      (empty)

upToNBlocks :: Int -> Parser [Word16]
upToNBlocks n = sepByAtMost n hextet (char ':')

--   HEXTET ::=
--      <hex> <hex> <hex> <hex>
--    | <hex> <hex> <hex>
--    | <hex> <hex>
--    | <hex>

hextet :: Parser Word16
hextet = fromIntegral <$> (integral 16 $ someAtMost 4 hexDigit)

--   HEX ::=
--      0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | a | b | c | d | e | f

--   Tokens / terminals
--      HEX DIGITS    ['0'..'f']
--      DOUBLE-COLON  "::"
--      COLON         ':'

doubleColon :: Parser Block
doubleColon = DoubleColon <$ string "::"

-- colon :: Parser Char
-- colon = char ':' <* notFollowedBy (char ':')

-------------------------------------------------------------------------------------
-- Create two hextets out of a Word32
hextetsOf :: Word32 -> [Word16]
hextetsOf x = reverse $ unfoldr f (x, 2)
  where
    f (w, c) | c == 0    = Nothing
             | otherwise = Just (fromIntegral w .&. 0xffff, (shiftR w 16, c - 1))

-- Assumptions, to be enforced by the parser as preconditions:
--    Input block list is one of:
--      - exactly 8 elements and DOES NOT contain a DoubleColon
--      - fewer than 8 elements and contains a SINGLE DoubleColon

blocksToAddress :: [Block] -> IPAddress6
blocksToAddress = toAddress . combine . reduce
  where
    -- TODO rename these subfunctions
    toAddress :: [Word16] -> IPAddress6
    toAddress ws = IPAddress6 (smush $ take 4 ws) (smush $ drop 4 ws)  -- TODO use splitAt?
      where smush = foldl' (\acc x -> (shiftL acc 16) .|. fromIntegral x) 0

    combine :: (Int, Bool, [Word16], [Word16]) -> [Word16]
    combine (n, _, lhs, rhs) = lhs ++ (replicate (8-n) 0) ++ rhs

    reduce :: [Block] -> (Int, Bool, [Word16], [Word16])
    reduce = foldr f (0, False, [], [])

  --    (
  --      Int         hextet counter
  --      Bool        double colon seen yet?
  --      [Word16]    words to left of double colon
  --      [Word16]    words to right of double colon
  --    )
    f :: Block -> (Int, Bool, [Word16], [Word16]) -> (Int, Bool, [Word16], [Word16])
    f DoubleColon (_, True,  _,   _)   = error "too many double colons in address"
    f DoubleColon (n, False, lhs, rhs) = (n,   True,  lhs,     rhs)
    f (Hextet h)  (n, False, lhs, rhs) = (n+1, False, lhs,     h : rhs)
    f (Hextet h)  (n, True,  lhs, rhs) = (n+1, True,  h : lhs, rhs)
    f (IP4Addr a) (n, False, lhs, rhs) = (n+2, False, lhs, hi4 a ++ rhs)
    f (IP4Addr a) (n, True,  lhs, rhs) = (n+2, True,  hi4 a ++ lhs, rhs)

    hi4 (IPAddress a) = hextetsOf a

-------------------------------------------------------------------------------------
-- Combinators

-- Transform a parser of digit characters into a parser of base-N integers
-- N.B. the base must be one that is handled by digitToInt (i.e. [2..16])
integral :: (Functor f, Foldable t) => Int -> f (t Char) -> f Int
integral base digits = foldl' (\x d -> base * x + digitToInt d) 0 <$> digits

-- TODO can we write manyAtMost and someAtMost
--      in terms of Alternative rather than Parsing?
--      This means giving up backtracking with `try`

-- many, but at most N
-- manyAtMost :: Parsing m => Int -> m a -> m [a]
-- manyAtMost 0 _ = pure []
-- manyAtMost n p = try (count n p) <|> (manyAtMost (n-1) p)

-- some, but at most N
someAtMost :: Parsing m => Int -> m a -> m [a]
someAtMost 0 _ = pure []
someAtMost 1 p = liftA2 (:) p (pure [])
someAtMost n p = try (count n p) <|> (someAtMost (n-1) p)

sepByAtMost :: Parsing m => Int -> m a -> m sep -> m [a]
sepByAtMost 0 _ _   = pure []
sepByAtMost n p sep = try (sepByN n p sep) <|> (sepByAtMost (n-1) p sep)

-- modified version of sepBy1 from Text.Parser.Combinators
sepByN :: Alternative m => Int -> m a -> m sep -> m [a]
sepByN 0 _ _   = pure []
sepByN n p sep = (:) <$> p <*> count (n-1) (sep *> p)

-- discard the results of a succesful parse
-- discard :: Functor f => f a -> f ()
-- discard p = () <$ p

-----------
-- used in early iterations

-- strictHextet :: Parser Word16
-- strictHextet = fromIntegral <$> hexNumN 4

-- hexNumN :: Int -> Parser Int
-- hexNumN n = foldl' (\x d -> 16 * x + digitToInt d) 0 <$> (count n hexDigit)
