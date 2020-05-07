module FoldBool where

foldBool :: a -> a -> Bool -> a
foldBool x y pred =
    case pred of
        False -> x
        True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y pred
  | pred      = y
  | otherwise = x

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y
