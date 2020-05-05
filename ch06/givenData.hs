module GivenData where
    
data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- question 1
--phew = Papu "chases" True

-- question 2
truth = Papu (Rocks "chomskydoz")
             (Yeah True)

-- question 3
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

-- question 4
--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'
