module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

-- Exercise: write 'map' for the binary tree

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)


testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected = 
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup OK!"
  else error "test failed!"
    
-- Exercise: convert binary tree to list, using three different traversal orders

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = concat [[x], preorder left, preorder right]

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = concat [inorder left, [x], inorder right]

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = concat [postorder left, postorder right, [x]]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testTree'' :: BinaryTree Integer
testTree'' =
  insert' 9
    $ insert' 8
    $ insert' 6
    $ insert' 1
    $ insert' 4
    $ insert' 2
    $ insert' 7 
    $ insert' 3
    $ insert' 5 Leaf

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears."

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- Exercise: write foldr for binary tree
-- Can we do this easily without converting the tree to a list first?

-- inorder
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left x right) = foldTree f z' left
  where
    z'  = f x z''
    z'' = foldTree f z right

foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' f z t = foldr f z (inorder t)

-- preorder
foldTreePre :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTreePre _ z Leaf = z
foldTreePre f z (Node left x right) = f x z' 
  where 
    z'  = foldTreePre f z'' left
    z'' = foldTreePre f z right

-- postorder
foldTreePost :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTreePost _ z Leaf = z
foldTreePost f z (Node left x right) = foldTreePost f z' left
  where
    z'  = foldTreePost f z'' right
    z'' = f x z
