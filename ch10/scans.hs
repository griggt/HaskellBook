module Scans where

fibs    = 1 : scanl (+) 1 fibs
fibsN n = fibs !! n

-- exercise 1
fibs20 = take 20 fs where 
    fs = 1 : scanl (+) 1 fs

-- exercise 2
fibsUnder100 = takeWhile (<100) fs where 
    fs = 1 : scanl (+) 1 fs
