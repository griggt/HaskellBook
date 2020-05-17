module TreeBuilder where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)


-- exercise #1
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f z = go Leaf z
  where 
    go t z = case f z of
      Nothing          -> t
      Just (l', v, r') -> (Node (go t l') v (go t r'))


-- exercise #2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where
    f x
      | x >= n    = Nothing
      | otherwise = Just (x + 1, x, x + 1)
