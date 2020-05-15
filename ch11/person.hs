module Person where

data Person =
    Person { name :: String
           , age :: Int }
           deriving (Eq, Show)

-- ERROR: can't define two records with same named field
-- data Cat = 
--     Cat { color :: String
--         , age :: Int }
--         deriving (Eq, Show)
