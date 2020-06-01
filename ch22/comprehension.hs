module Comprehension where

newtype Reader r a = Reader { runReader :: r -> a }    

-- exercise #1
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

-- exercise #2
asks :: (r -> a) -> Reader r a
asks f = Reader f