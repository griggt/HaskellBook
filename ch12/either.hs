module Either where

-- In regards to exercises 1 and 2, the standard library version
--  of lefts and rights uses a list comprehension rather than foldr

-- exercise 1
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left x) y  = x : y
        f (Right _) y = y

-- exercise 2
rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Left _) y  = y
        f (Right x) y = x : y

-- exercise #3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = go x [] []
  where 
    go []           ls rs = (ls, rs)
    go (Left x:xs)  ls rs = go xs (x:ls) rs
    go (Right x:xs) ls rs = go xs ls (x:rs)

-- exercise #4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just (f x)

-- exercise #5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

-- exercise #6
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
