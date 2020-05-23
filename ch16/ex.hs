module Exercises where

-- exercise #5, pg 647

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
    in fmap (*3) changed
