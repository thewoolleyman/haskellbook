module RollYourOwn where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = rollsToGetN 20


rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit = go 0 0
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= limit = count
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Int])
rollsCountLogged limit g = go 0 0 g []
  where go :: Int -> Int -> StdGen -> [Int] -> (Int, [Int])
        go sum count gen acc
          | sum >= limit = (count, acc)
          | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
            in go (sum + die) (count + 1) nextGen (die : acc)
