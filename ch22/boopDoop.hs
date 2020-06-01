module BoopDoop where

import Control.Applicative

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

-- desugar the do syntax
boopDoop' :: Integer -> Integer
boopDoop' = 
  boop >>=
    \a ->
      doop >>=
        \b ->
          return (a + b)

--- If we split this into:
---   boop >>= x
--  then 
--     boop :: Integer -> Integer
--        x :: Integer -> Integer -> Integer
--    (>>=) :: Monad m => m a -> (a -> m b) -> m b
--      m a :: Integer -> Integer
--      m b :: Integer -> Integer
--        a :: Integer
--             (Integer -> Integer) -> (Integer -> Integer -> Integer) -> (Integer -> Integer)
--   ergo the types check

--  beta reduce by applying the bind function to see the flow of the arguments
boopDoop'' :: Integer -> Integer
boopDoop'' =
    \s ->
      (\a ->
        \r -> 
          (\b -> 
            return (a + b))
          (doop r) 
          r) 
      (boop s) 
      s
