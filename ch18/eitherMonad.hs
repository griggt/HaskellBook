module EitherMonad where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x)  = First x
  fmap f (Second y) = Second (f y)

instance Applicative (Sum a) where
  pure = Second
  First f  <*> _ = First f
  Second f <*> x = fmap f x

-- (>>=) :: Monad m 
--       => m a -> (a -> m b) -> m b
instance Monad (Sum a) where
  First x  >>= _  = First x
  Second x >>= f  = f x
