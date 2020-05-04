module Reverse where

rvrs :: String -> String
rvrs x = 
    concat [w1, w2, w3]
    where w1 = drop 9 x
          w2 = take 4 $ drop 5 x
          w3 = take 5 x

main :: IO ()
main = print (rvrs "Curry is awesome")
