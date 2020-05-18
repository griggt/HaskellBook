module Palindrome where

import Control.Monad
import Data.Char (isLetter, toLower)
import Data.List (partition)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
      True -> putStrLn "It's a palindrome"
      False -> do 
          putStrLn "Nope!"
          exitSuccess

isPalindrome :: String -> Bool
isPalindrome x = letters == reverse(letters)
  where letters = map toLower . filter isLetter $ x

palindrome' :: IO ()
palindrome' = forever $ do
  line1 <- getLine
  case isPalindrome line1 of
      True -> putStrLn "It's a palindrome"
      False -> do 
          putStrLn "Nope!"
          exitSuccess
