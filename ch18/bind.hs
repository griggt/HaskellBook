module Bind where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join . fmap f $ x

-- simple check

andOne :: Int -> [Int]
andOne x = [x, 1]

main :: IO ()
main = do
  putStrLn $ show $ bind andOne [4, 5, 6]
  putStrLn $ "[4,1,5,1,6,1] is the correct result"
