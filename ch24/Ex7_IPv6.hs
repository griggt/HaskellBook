module IPv6Address where

import Control.Applicative (liftA2, Alternative, (<|>))
import Data.Bits ((.|.), (.&.), shiftL, shiftR, finiteBitSize, Bits, FiniteBits)
import Data.Char (digitToInt, intToDigit)
import Data.List (intercalate, unfoldr)
import Data.Foldable (foldl')
import Data.Word (Word16, Word64)
import Numeric (showHex)
import Test.QuickCheck (quickCheck, withMaxSuccess, Arbitrary(..))
import Text.Trifecta hiding (colon)

import Ex6_IPv4 (IPAddress(..), showIPAddress, ipaddr')

data IPAddress6 = IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)

data Block =
    Hextet Word16
  | DoubleColon
  | IP4Addr IPAddress
  deriving (Eq, Show)

--------------------------------------------------------------
-- exercise #9

fromIPAddress :: IPAddress -> IPAddress6
fromIPAddress (IPAddress w32) = IPAddress6 0 $ (fromIntegral w32) .|. transitionPrefix
  where transitionPrefix = shiftL 0xffff 32

--------------------------------------------------------------

instance Arbitrary IPAddress6 where
  arbitrary = liftA2 IPAddress6 arbitrary arbitrary

testParse :: (IPAddress6 -> String) -> IPAddress6 -> Bool
testParse stringify addr =
  case parseString ipv6Address mempty (stringify addr) of
    Failure _ -> False
    Success a -> a == addr

do_quickcheck :: IO ()
do_quickcheck = do
  quickCheck (withMaxSuccess 1000 $ testParse canonical)
  quickCheck (withMaxSuccess 1000 $ testParse abbreviated)
  quickCheck (withMaxSuccess 1000 $ testParse ipv4transitional)

--------------------------------------------------------------
-- Formatting as string  (exercise #8)

canonical :: IPAddress6 -> String
canonical (IPAddress6 uw lw) = (hexword uw) ++ ":" ++ (hexword lw)
  where hexword w = intercalate ":" . map (showHexP 4) . hextetsOf $ w

canonical' :: IPAddress6 -> String
canonical' (IPAddress6 uw lw) = (hexword uw) ++ ":" ++ (hexword lw)
  where    
    hexword w = foldr f "" (hextetsOf w)
    f w "" = sh w
    f w s  = (sh w) ++ (':' : s)
    sh = showHexP 4

abbreviated :: IPAddress6 -> String
abbreviated (IPAddress6 uw lw) = abbreviate ws blocks
  where
    ws = (hextetsOf uw) ++ (hextetsOf lw)
    blocks = Hextet <$> ws

ipv4transitional :: IPAddress6 -> String
ipv4transitional (IPAddress6 uw lw) = abbreviate ws blocks
  where
    ws = take 6 $ (hextetsOf uw) ++ (hextetsOf lw)
    i4 = IPAddress $ fromIntegral lw .&. 0xffffffff
    blocks = (Hextet <$> ws) ++ [IP4Addr i4]

abbreviate :: [Word16] -> [Block] -> String
abbreviate hextets blocks =
  case findLongestZeroRun hextets of
    (rs, rl) | rl > 1    -> showBlocks . spliceDC (rs,rl) $ blocks
             | otherwise -> showBlocks $ blocks
  where spliceDC run = cutAndSplice run [DoubleColon]

-- TODO use ShowS rather than all the (++)
-- TODO have `showHex` function be a param so I can choose padded or not
-- Trick to get a String out of a [ShowS] z:  (foldr (.) id z) ""
--  z' = map (\x -> showHex x . showString ":") z
-- To keep transform [ShowS] -> [String]:     map ($ "") z'
showBlocks :: [Block] -> String
showBlocks = foldr f ""
  where
    f :: Block -> String -> String
    f DoubleColon xs         = "::" ++ xs
    f (Hextet h)  ""         = showHex h ""
    f (Hextet h)  xs@(':':t) = showHex h ""  ++ xs
    f (Hextet h)  xs         = showHex h ":" ++ xs
    f (IP4Addr ip4) xs       = showIPAddress ip4 ++ xs

-- Convert integer 'x' into a hexedecimal string zero-padded to width
-- Note that if the number is to large to fit into 'width' digits, the most
-- significant digits will not be displayed, as the output is always 'width'
-- characters in length.
showHexP :: (Integral a, Bits a) => Int -> a -> String
showHexP width = unfoldIntegral width 4 (intToDigit . fromIntegral)

-----------------------------------------------------------------------------
--  to find longest run:
--    type Run = (Int, Int)   -- start index, length
--    run a foldl' with accumulator of the following structure
--       (
--         Run -- longest run
--         Run -- current run
--         Int -- current position counter
--       )
--  then use a cutAndSplice to replace the run with a double colon

type Run = (Int, Int)

findLongestZeroRun :: Integral a => [a] -> Run
findLongestZeroRun = longest . foldl' f ((0,0), (0,0), 0)
  where
    longest (r, _, _) = r
    maxRun (s,n) (s',n') = if (n' > n) then (s',n') else (s,n)

    f :: Integral a => (Run, Run, Int) -> a -> (Run, Run, Int)
    f (lng, (0,0), i) 0 = let c = (i,1)   in (maxRun lng c, c, i+1)
    f (lng, (s,n), i) 0 = let c = (s,n+1) in (maxRun lng c, c, i+1)
    f (lng, _,     i) _ = let c = (0,0)   in (lng,          c, i+1)

-- arguments: Run, text to splice, existing text
cutAndSplice :: Run -> [a] -> [a] -> [a]
cutAndSplice (i, n) s xs = lhs ++ s ++ (drop n rhs)
  where (lhs, rhs) = splitAt i xs

---------------------------------------------------------------
-- for notational convenience
(<<$>>) = fmap . fmap

----------------------------------------------------------------
--   ** BNF GRAMMAR **
----------------------------------------------------------------

--   IPV6-ADDRESS ::=
--      <canonical-address>
--    | <abbreviated-address>
--    | <ipv4-transitional-address>

ipv6Address :: Parser IPAddress6
ipv6Address = try canonicalAddress <|> try ipv4TransitionalAddress <|> abbreviatedAddress

--   CANONICAL-ADDRESS ::=
--      <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet>

canonicalAddress :: Parser IPAddress6
canonicalAddress = blocksToAddress <$> Hextet <<$>> sepByN 8 hextet colon <?> "canonical address"

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
-- 7    <up-to-six-hextets>   "::"
-- 6  | <up-to-five-hextets>  "::" hextet
-- 5  | <up-to-four-hextets>  "::" hextet ':' hextet
-- 4  | <up-to-three-hextets> "::" hextet ':' hextet ':' hextet
-- 3  | <up-to-two-hextets>   "::" hextet ':' hextet ':' hextet ':' hextet
-- 2  | <up-to-one-hextet>    "::" hextet ':' hextet ':' hextet ':' hextet ':' hextet
-- 1  | <zero-hextets>        "::" hextet ':' hextet ':' hextet ':' hextet ':' hextet ':' hextet

--    TODO can I perform less backtracking if I put the variable length
--         block runs on the right hand side of the '::' ?

abbreviatedAddress :: Parser IPAddress6
abbreviatedAddress = blocksToAddress <$> choice (reverse abbrevs) <?> "abbreviated address"  -- TODO ugggh, reverse
  -- argument `n' is number of fixed blocks to right of double colon
  where abbrevs = [try $ abbrevBlocks n 6 False | n <- [0..6]]

abbrevBlocks :: Int -> Int -> Bool -> Parser [Block]
abbrevBlocks n m fl = Hextet <<$>> upToNHextets (m - n)
                   <> (pure   <$>  doubleColon)
                   <> Hextet <<$>> sepByC n hextet colon
  where sepByC = if fl then endByN else sepByN

--   IPV4-TRANSITIONAL-ADDRESS ::=
--      <canonical-ipv4-transitional-address>
--    | <abbreviated-ipv4-transitional-address>

ipv4TransitionalAddress :: Parser IPAddress6
ipv4TransitionalAddress = try canonicalIPv4Transitional <|> abbreviatedIPv4Transitional 
                            <?> "IPv4 transitional address"

--   CANONICAL-IPV4-TRANSITIONAL-ADDRESS ::=
--      <canonical-6-hextet> <ipv4-address>

canonicalIPv4Transitional :: Parser IPAddress6
canonicalIPv4Transitional = do
  lhs <- canonical6Hextet
  rhs <- ipaddr'
  return (blocksToAddress $ (Hextet <$> lhs) ++ [IP4Addr rhs]) <?> "canonical IPv4 transitional address"

--   ABBREVIATED-IPV4-TRANSITIONAL-ADDRESS ::=
--      <abbreviated-6-hextet> <ipv4-address>

abbreviatedIPv4Transitional :: Parser IPAddress6
abbreviatedIPv4Transitional = do
  lhs <- abbreviated6Hextet
  rhs <- ipaddr'
  return (blocksToAddress (lhs ++ [IP4Addr rhs])) <?> "abbreviated IPv4 transitional address"

--   CANONICAL-6-HEXTET ::=
--      <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':' <hextet> ':'

canonical6Hextet :: Parser [Word16]
canonical6Hextet = endByN 6 hextet colon <?> "canonical 6-hextet"

--   ABBREVIATED-6-HEXTET ::=
--      <up-to-four-hextets>   "::"
--    | <up-to-three-hextets>  "::" hextet ':'
--    | <up-to-two-hextets>    "::" hextet ':' hextet ':'
--    | <up-to-one-hextet>     "::" hextet ':' hextet ':' hextet ':'
--    | <zero-hextets>         "::" hextet ':' hextet ':' hextet ':' hextet ':'

abbreviated6Hextet :: Parser [Block]
abbreviated6Hextet = choice (reverse abbrevs) <?> "abbreviated 6-hextet"  -- TODO ugggh, reverse
  where abbrevs = [try $ abbrevBlocks n 4 True | n <- [0..4]]

--   UP-TO-SIX-HEXTETS ::=
--      <hextet> ':' <up-to-five-hextets>
--    | <up-to-five-hextets>

--   UP-TO-FIVE-HEXTETS ::=
--      <hextet> ':' <up-to-four-hextets>
--    | <up-to-four-hextets>

--   UP-TO-FOUR-HEXTETS ::=
--      <hextet> ':' <up-to-three-hextets>
--    | <up-to-three-hextets>

--   UP-TO-THREE-HEXTETS ::=
--      <hextet> ':' <up-to-two-hextets>
--    | <up-to-two-hextets>

--   UP-TO-TWO-HEXTETS ::=
--      <hextet> ':' <up-to-one-hextet>
--    | <up-to-one-hextet>

--   UP-TO-ONE-HEXTETS ::=
--      <hextet>
--    | <zero-hextets>

--   ZERO-HEXTETS ::=
--      (empty)

upToNHextets :: Int -> Parser [Word16]
upToNHextets n = sepByAtMost n hextet colon -- <?> ("up to " ++ (show n) ++ " hextets")

--   IPV4-ADDRESS ::=
--      <octet> '.' <octet> '.' <octet> '.' <octet>

--   NB For parsing the IPv4 address portion we use `ipaddr'` imported from Ex6_IPv4

--   HEXTET ::=
--      <hex> <hex> <hex> <hex>
--    | <hex> <hex> <hex>
--    | <hex> <hex>
--    | <hex>

hextet :: Parser Word16
hextet = fromIntegral <$> (integral 16 $ someAtMost 4 hexDigit) <?> "hextet"

--   OCTET ::=
--      <decimal>       -- in range [0..255]

--   HEX ::=
--      0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | a | b | c | d | e | f

--   Tokens / terminals
--      HEX DIGITS    ['0'..'f']
--      DOUBLE-COLON  "::"
--      COLON         ':'

doubleColon :: Parser Block
doubleColon = DoubleColon <$ string "::"

colon :: Parser Char
colon = char ':' -- <* notFollowedBy (char ':')

-------------------------------------------------------------------------------------
-- Assumptions, to be enforced by the parser as preconditions:
--    Input block list is one of:
--      - exactly 8 elements and DOES NOT contain a DoubleColon
--      - fewer than 8 elements and contains a SINGLE DoubleColon
blocksToAddress :: [Block] -> IPAddress6
blocksToAddress = toAddress . combine . reduce
  where
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

-- Create four hextets out of a Word64
hextetsOf :: (FiniteBits a, Integral a) => a -> [Word16]
hextetsOf x = unfoldIntegral (finiteBitSize x `div` 16) 16 fromIntegral x

unfoldIntegral :: (Integral a, Bits a) =>
                  Int ->        -- # of output items
                  Int ->        -- # of bits per output item
                  (a -> b) ->   -- transformation
                  a ->          -- input integral
                  [b]
unfoldIntegral n k g x = reverse $ unfoldr f (x, n)
  where
    mask = 2 ^ k - 1
    f (x, c) | c == 0    = Nothing
             | otherwise = Just (g $ x .&. mask, (shiftR x k, c - 1))

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
sepByAtMost n p sep = try (sepByN n p sep)<|> (sepByAtMost (n-1) p sep)

-- modified version of sepBy1 from Text.Parser.Combinators
sepByN :: Alternative m => Int -> m a -> m sep -> m [a]
sepByN 0 _ _   = pure []
sepByN n p sep = (:) <$> p <*> count (n-1) (sep *> p)

-- variant of endBy1 with a specified count; for symmetry with sepByN
endByN :: Alternative m => Int -> m a -> m sep -> m [a]
endByN n p sep = count n (p <* sep)

sepBy1 p sep = (:) <$> p <*> many (sep *> p)
endBy1 p sep = some (p <* sep)

-- discard the results of a succesful parse
-- discard :: Functor f => f a -> f ()
-- discard p = () <$ p
