module Control.DivideAndConquer where

import Control.Parallel
import Data.Vector
import GHC.Conc



-- example for paper: Merge sort


-- map each prob to a processor and have a work stealing queue
-- this is the point of parallelism
-- Currently Par Monad style parMap
-- Or using strategies
-- parMapVec f v = runEval $ evalTraversable rpar $ V.map f v

-- parMap :: (prob -> sol) -> Vector prob -> Vector sol
-- parMap f probs = undefined


-- | Zero Assignment Parallel Processor skeleton

divideAndConquer :: (Parallelizable t)
                 => (prob -> Bool)        -- indivisibility test
                 -> (prob -> t prob) -- split
                 -> (t sol -> sol)   -- join
                 -> (prob -> sol)         -- the function to be applied
                 -> prob
                 -> sol
divideAndConquer indivisible split join f = func
  where func problem
          | indivisible problem = f problem
          | otherwise     = (join . parMap f . split) problem


-- | Fixed Degree Divide And Conquer

type K = Int

fixedDivideAndConquer :: (Parallelizable t)
                      => K -- number of subproblems in each split
                      -> (prob -> Bool) -- indivisibility test
                      -> (K -> prob -> t prob) -- split -- O(1) with vectors
                      -> (K -> t sol -> sol)   -- join  -- Can this be O(1)? Should be O(1) for arrays
                      -> (prob -> sol)              -- the function to be applied
                      -> prob
                      -> sol
fixedDivideAndConquer k indivisible split join f = func
  where func problem
          | indivisible problem = f problem
          | otherwise = (join k . parMap f . split k) problem

-- Can we have some fusion rule/deforestation for `join . parMap f . split`
-- How to make (++) O(1)
-- Can linear types make (++) O(1)
-- What is the form of parallelism that we leverage inside parMap. Should experiment with vectorization!


-- (++) always seems to be O(m+n) because both the vectors need to be copied to a new vector
-- What if we can make amortized O(1)
-- We statically know the size of the array and have a head pointer so *last = *head + size
-- so instead of copying keep *head as constant and instead of a consistent size now have another pointer *next = head of the next array
-- when calculating the size, traverse the chain of pointers until *next = null and recursively add size
-- and append is just traversing the chain of next pointers until it hits null and then point that pointer to the head of the second array
