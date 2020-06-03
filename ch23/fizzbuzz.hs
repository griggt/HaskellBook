module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod`  5 == 0 = "Buzz"
           | n `mod`  3 == 0 = "Fizz"
           | otherwise       = show n


-- direct output

normal :: IO ()
normal = mapM_ (putStrLn . fizzBuzz) [1..100]          

---
-- using state w/list

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

asList :: IO ()
asList = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]

---
-- using state w/difference list

fizzbuzzDList :: [Integer] -> DL.DList String
fizzbuzzDList list =
  execState (mapM_ addResultDL list) DL.empty
  
addResultDL :: Integer -> State (DL.DList String) ()
addResultDL n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzDList [1..100]

---
-- exercise

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo lo hi = fizzbuzzList [hi,(pred hi)..lo]
