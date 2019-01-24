module DivideAndConquer where

import Data.Vector


par_map :: (prob -> sol) -> Vector prob -> Vector sol
par_map f probs = undefined

-- | Zero Assignment Parallel Processor skeleton

divideAndConquer :: (prob -> Bool)        -- indivisibility test
                 -> (prob -> Vector prob) -- split
                 -> (Vector sol -> sol)   -- join
                 -> (prob -> sol)         -- the function to be applied
                 -> prob
                 -> sol
divideAndConquer indivisible split join f = func
  where func problem
          | indivisible problem = f problem
          | otherwise     = (join . par_map f . split) problem


-- | Fixed Degree Divide And Conquer

type K = Int

fixedDivideAndConquer :: K -- number of subproblems in each split
                      -> (prob -> Bool) -- indivisibility test
                      -> (K -> prob -> Vector prob) -- split
                      -> (K -> Vector sol -> sol)   -- join
                      -> (prob -> sol)              -- the function to be applied
                      -> prob
                      -> sol
fixedDivideAndConquer k indivisible split join f = func
  where func problem
          | indivisible problem = f problem
          | otherwise = (join k . par_map f . split k) problem

-- Can we have some fusion rule/deforestation for `join . par_map f . split`
