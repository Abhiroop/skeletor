module Control.DivideAndConquer where

import Control.Applicative
import Control.Monad
import Control.Parallel
import Data.Foldable (foldl')


-- example for paper: Merge sort


-- map each prob to a processor and have a work stealing queue
-- this is the point of parallelism
-- Currently Par Monad style parMap
-- Or using strategies
-- parMapVec f v = runEval $ evalTraversable rpar $ V.map f v

-- parMap :: (prob -> sol) -> Vector prob -> Vector sol
-- parMap f probs = undefined


-- | Fixed Degree Divide And Conquer

{-# INLINE mapSkel #-}
mapSkel :: (Parallelizable t, Functor t, Foldable m, Monoid (t b), Eq (t a))
        => K -- number of subproblems in each split for the parallel workload
        -> (t b -> t b -> t b) -- parallel merge
        -> (t b -> t b -> t b) -- sequential merge
        -> (t a -> m (t a))    -- sequential split
        -> (t a -> Bool)       -- divide further?
        -> (a -> b)            -- a general function mostly use id for same datatype
        -> t a
        -> t b
mapSkel k parMerge seqMerge seqSplit continue f
  = parJoin parMerge . parSplit k func
  where
    func ta
      | continue ta = fmap f ta
      | otherwise   = let mta = seqSplit ta
                       in foldl' (\u ta' -> if ta' == ta
                                            then u `seqMerge` (fmap f ta) -- fix point reached
                                            else u `seqMerge` (func ta')) mempty mta

{-# INLINE mapSkel' #-}
mapSkel' :: (Parallelizable t)
                      => K -- number of subproblems in each split for the parallel workload
                      -> (t b -> t b -> t b) -- the sequential merge operator for parallel workload
                      -> (t a -> t b)  -- the function to be applied
                      -> t a
                      -> t b
mapSkel' k merge f xs = parJoin merge $ parSplit k f xs

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



-- parJoin parMerge . parSplit k (liftA2 f $ ta)

--------------------------------------------------------------------------------------
----------------- D&C redesign-------------------





dcA :: (Parallelizable t, Monoid (t b))
    => (a -> Bool)
    -> (a -> b)
    -> (a -> t a)
    -> (t b -> b)
    -> a
    -> b
dcA isTrivial basic split combine = r
  where
    r x
      | isTrivial x = basic x
      | otherwise   = (combine . pmap r . split) x


-- msort :: [Int] -> [Int]
-- msort = dcA isTrivial basic split combine
--   where
--     isTrivial xs = length xs <= 1
--     basic     = id
--     split xs  = [take (half xs) xs, drop (half xs) xs]
--     combine   = foldr1 merge
--     half   xs = length xs `div` 2
--     merge [] [] = []
--     merge [] ys = ys
--     merge xs [] = xs
--     merge f@(x:xs) l@(y:ys)
--       | x < y = x : (merge xs l)
--       | otherwise = y : (merge f ys)
