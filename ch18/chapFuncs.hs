module ChapterExFuncs where

---------------------------------------------------------
-- exercise #1
j :: Monad m => m (m a) -> m a
j x = x >>= id

-- Type checking:
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- id    :: c -> c

-- \x ->   x       >>=      id     -> RESULT
--         m a          (a -> m b) -> m b
--         m a          (c  -> c)  -> m b 
--              c === m b === a
--         m (m b)      (a -> a)   -> m b
--                       given
--         m (m b)                 -> m b

-- Thought process:
--   (1) >>= f is join . fmap f
--   (2) if f is id then that term has no effect
--        and we are left with just join

---------------------------------------------------------
-- exercise #2
-- this is liftM
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

---------------------------------------------------------
-- exercise #3
-- this is liftM2
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

-- can also be written using do-notation
l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f m1 m2 = do
  x <- m1         -- x is type a
  y <- m2         -- y is type b
  return (f x y)  -- f is type a -> b -> c, so result is m c

-- which desugars to
l2'' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2''  f m1 m2 =
  m1 >>=
    \x ->
      m2 >>=
        \y ->
          return (f x y)

---------------------------------------------------------
-- exercise #4
-- this is flip ap
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- can also be written using do-notation
a' :: Monad m => m a -> m (a -> b) -> m b
a' m1 m2 = do
  x <- m1       -- x is type a
  y <- m2       -- y is type a -> b
  return (y x)  -- so result is m b (after the function application)

-- which desugars to
a'' :: Monad m => m a -> m (a -> b) -> m b
a'' m1 m2 = 
  m1 >>= 
    \x1 -> 
      m2 >>=
        \x2 ->
          return (x2 x1)

---------------------------------------------------------
-- exercise #5

-- This is equivalent to 
--   meh xs f = sequence $ fmap f xs
-- or also
--   meh = flip mapM

-- but we don't know about sequence yet, so how else can we do 
--    [m a] -> m [a]  ?

-- I can generate a single element m [b] using
--    fmap pure $ f x
--      since f x :: m b
--    then fmap pure over that gives us m [b]
--       (since [] is Applicative)
-- where x is the head of the input list.

-- Then I just need a combining function:
--   ([a] -> [a] -> [a]) -> m [a] -> m [a] -> m [a]
--   e.g. sort-of-zipWith for monads, e.g. liftM2/A2, e.g. l2 above

-- Interestingly, mapM is actually implemented using foldr rather than
-- explicit recursion, and do-notation for the body. A very different 
-- style to what I have written here, and likely more idiomatic Haskell.
-- Also see code comments on implementation optimization for sequence/mapM.

-- However I believe what I have written will work for Applicatives
-- as well as Monads?

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _     = pure []
meh (x:xs) f = l2 (++) (fmap pure $ f x) (meh xs f)

---------------------------------------------------------
-- exercise #6

-- This is `sequence` from the Prelude

-- If we pass `id` as the function to `meh` above, knowing that:
--   meh xs f = sequence $ fmap f xs
-- then
--   meh xs id = sequence $ fmap id xs
--             = sequence $ xs
-- which is what we are looking for.

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id
