module Person where

import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | not (age > 0)         = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

-----------------------

-- 1) prompt the user for name and age inputs
-- 2) attempt to construct a Person using these values
-- 3) print success or failure message as specified

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Enter a name: "
  name <- getLine

  putStr "Enter an age: "
  ageStr <- getLine
  age <- return (read ageStr :: Integer)

  case mkPerson name age of
      Right p        -> putStrLn ("Yah! Got a person: " ++ show p)
      Left NameEmpty -> putStrLn "Missing a name!"
      Left AgeTooLow -> putStrLn "Too young!"
      Left (PersonInvalidUnknown s) -> putStrLn ("Unexpected error: " ++ s)
