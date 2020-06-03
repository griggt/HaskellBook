module ChapterExercises where

import Moi

-- TODO review the actual implementations in the State monad

------------------------------------------------------
-- exercise 1
get :: Moi s s
get = Moi $ \x -> (x, x)

------------------------------------------------------
-- exercise 2
put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

------------------------------------------------------
-- exercise 3
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd . sa $ s

------------------------------------------------------
-- exercise 4
eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa

------------------------------------------------------
-- exercise 5
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \x -> ((), f x)
