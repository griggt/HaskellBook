{-# LANGUAGE MultiWayIf #-}  -- just for fun

module Ex6_IPv4 where

import Control.Applicative
import Data.Bits
import Data.Foldable
import Data.List
import Data.Word
import Text.Trifecta

data IPAddress = IPAddress Word32
  deriving (Eq, Ord, Show)

ipaddr :: Parser IPAddress
ipaddr = IPAddress <$> combineOctets <$> sepByN 4 octet (char '.')
  where combineOctets [a, b, c, d] = shiftL (fromIntegral a) 24
                                 .|. shiftL (fromIntegral b) 16
                                 .|. shiftL (fromIntegral c)  8
                                 .|.        (fromIntegral d)

-- More concise, using fold. More Haskell-ish, less C-ish.
ipaddr' :: Parser IPAddress
ipaddr' = IPAddress <$> combineOctets <$> sepByN 4 octet (char '.') <?> "IPv4 address"
  where combineOctets = foldl' (\acc x -> (shiftL acc 8) .|. fromIntegral x) 0

octet :: Parser Word8
octet = do
  v <- decimal
  if | v > 255   -> fail "octet must be in the range [0..255]"
     | otherwise -> return $ fromIntegral v

-- modified version of sepBy1 from Text.Parser.Combinators
sepByN :: Alternative m => Int -> m a -> m sep -> m [a]
sepByN n p sep = (:) <$> p <*> count (n-1) (sep *> p)
