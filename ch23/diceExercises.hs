module DiceExercises where

import System.Random
import RandomExample

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sum count rolls gen
      | sum >= n = (count, rolls)
      | otherwise =
        let (r, nextGen) = randomR (1, 6) gen
        in go (sum + r) (count + 1) (intToDie r : rolls) nextGen
