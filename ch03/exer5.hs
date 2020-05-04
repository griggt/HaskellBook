module Exercise5 where

rvrs :: String
rvrs =
    concat [w1, w2, w3]
    where inputStr = "Curry is awesome"
          w1 = drop 9 inputStr
          w2 = take 4 $ drop 5 inputStr
          w3 = take 5 inputStr
