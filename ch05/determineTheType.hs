{-# LANGUAGE NoMonomorphismRestriction  #-}

module DetermineTheType where

example = 1

a1 = (* 9) 6
a2 = head [(0, "doge"), (1, "kitteh")]
a3 = head [(0 :: Integer, "doge"), (1, "kitteh")]
a4 = if False then True else False
a5 = length [1, 2, 3, 4, 5]
a6 = (length [1, 2, 3, 4]) > (length "TACOCAT")

x = 5
y = x + 5
w = y * 10

z y = y * 10
f = 4 / y

g = x ++ y ++ z 
    where 
        x = "Julie"
        y = " <3 "
        z = "Haskell"
