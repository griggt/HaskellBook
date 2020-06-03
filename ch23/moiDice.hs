module MoiDice where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import System.Random

import Moi
import RandomExample

---

rollDie :: Moi StdGen Die
rollDie = intToDie <$> moi (randomR (1, 6))

rollThrice :: Moi StdGen (Die, Die, Die)
rollThrice = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> Moi StdGen [Die]
nDie n = replicateM n rollDie
