module Exercise4 where

letterIndex :: Int -> Char
letterIndex x =
    inputStr !! x
    where inputStr = "Curry is awesome!"
