module Control.DivideAndConquer where

import Control.Parallel
import Control.Monad.Par (NFData)
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


-- | Fixed Degree Divide And Conquer

fixedDivideAndConquer :: (Parallelizable t)
                      => K -- number of subproblems in each split for the parallel workload
                      -> (t b -> t b -> t b) -- the sequential merge operator for parallel workload
                      -> (t a -> t b)  -- the function to be applied
                      -> t a
                      -> t b
fixedDivideAndConquer k merge f = parJoin merge . parSplit k f

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
