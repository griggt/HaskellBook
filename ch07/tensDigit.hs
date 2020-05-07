module TensDigit where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (t, _) = divMod x 10
        (_, d) = divMod t 10

hunsD :: Integral a => a -> a
hunsD x = d
  where (s, _) = divMod x 10
        (t, _) = divMod s 10
        (_, d) = divMod t 10
