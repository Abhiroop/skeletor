module Bench.Sort where

import Data.Vector
import Data.List
import Control.DivideAndConquer
import System.Random (randomRIO)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs)

foo :: IO ()
foo = do
  l <- randomList 1000
  print $ sort l


-- fixedDivideAndConquer 10 
-- fixedDivideAndConquer :: (Parallelizable t, NFData sol)
--                       => K -- number of subproblems in each split
--                       -> (prob -> Bool) -- indivisibility test
--                       -> (K -> prob -> t prob) -- split -- O(1) with vectors
--                       -> (K -> t sol -> sol)   -- join  -- Can this be O(1)? Should be O(1) for arrays
--                       -> (prob -> sol)              -- the function to be applied
--                       -> prob
--                       -> sol
