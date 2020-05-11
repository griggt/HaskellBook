module StdFuncs where

-- exercise #1
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

-- exercise #2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

-- exercise #3
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem x (y:ys) = (x == y) || myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x xs = any (==x) xs

-- exercise #4
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- exercise #5
squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

-- exercise #6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- exercise #7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- exercise #8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ []     = error "empty list"
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) = 
  case f x rest of
      GT -> x
      EQ -> x
      LT -> rest
  where rest = myMaximumBy f xs

-- exercise #9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ []     = error "empty list"
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:xs) =
  case f x rest of
      LT -> x
      EQ -> x
      GT -> rest
  where rest = myMinimumBy f xs

-- exercise #10
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
