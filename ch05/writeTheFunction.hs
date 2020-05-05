module WriteTheFunction where

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c' :: a -> b -> b
c' x y = y

-- r x = reverse x
r :: [a] -> [a]
r [] = []
r (x:xs) = (r xs) ++ [x]

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)

a :: (a -> c) -> a -> a
a aToC x = x

a' :: (a -> b) -> a -> b
a' aToB x = aToB x
