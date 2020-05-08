module EnumFromTo where

-- if == a b, return single element list
-- if b < a, return empty list
-- otherwise return [a] ++ eft succ a ?    

eftBool :: Bool -> Bool -> [Bool]
eftBool from to 
  | from > to  = []
  | from == to = [from]
  | otherwise  = [from] ++ eftBool (succ from) to

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to
  | from > to = []
  | from == to = [from]
  | otherwise  = [from] ++ eftOrd (succ from) to

eftInt :: Int -> Int -> [Int]
eftInt from to
  | from > to = []
  | from == to = [from]
  | otherwise  = [from] ++ eftInt (succ from) to

eftChar :: Char -> Char -> [Char]
eftChar from to
  | from > to = []
  | from == to = [from]
  | otherwise  = [from] ++ eftChar (succ from) to

-- General purpose case, we need Enum and Ord typeclasses
eftAny :: (Enum a, Ord a) => a -> a -> [a]
eftAny from to
  | from > to  = []
  | from == to = [from]
  | otherwise  = from : eftAny (succ from) to
