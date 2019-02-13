{-# LANGUAGE RankNTypes #-}
module Bench.Sort where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as M
import Data.List
import Debug.Trace
import Control.DivideAndConquer
import System.Random (randomRIO)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs)

bench1 :: IO ()
bench1 = do
  l <- randomList 100000
  print $ "Starting sort for list of length " ++ show (length l)
  let --vec   = mergesort (V.fromList l)
      -- vec   = sortSkelM (V.fromList l)
      vec   = msortSkel (V.fromList l)
  --print vec
  print $ V.length vec


parallelWorkLoad :: Int
parallelWorkLoad = 1000
-------------------------------------------------------------------------------------
-- Merge sort

merge :: V.Vector Int -> V.Vector Int -> V.Vector Int
merge xs ys
  | length xs == 0 = ys
  | length ys == 0 = xs
  | otherwise
    = let h1 = V.head xs
          h2 = V.head ys
          t1 = V.tail xs
          t2 = V.tail ys
       in if h1 < h2
          then V.cons h1 (merge t1 ys)
          else V.cons h2 (merge xs t2)

-- Merge sort sequential
mergesort :: V.Vector Int -> V.Vector Int
mergesort xs
  | V.length xs < 2 = xs
  | otherwise = let (first,second) = V.splitAt half xs
                in merge (mergesort first) (mergesort second)
  where
    half = V.length xs `div` 2

-- Merge sort parallel
sortSkelM :: V.Vector Int -> V.Vector Int
sortSkelM = fixedDivideAndConquer parallelWorkLoad merge mergesort

-- Merge sort parallel and more general
msortSkel :: V.Vector Int -> V.Vector Int
msortSkel = fixedDivideAndConquer' parallelWorkLoad merge merge m_partition (\x -> V.length x < 2) id
  where
    m_partition xs
      = let (first, second) = V.splitAt (V.length xs `div` 2) xs
         in V.fromList [first,second]

--------------------------------------------------------------------------
-- Quick sort

-- Quick sort sequential
qsort :: V.Vector Int -> V.Vector Int
qsort xs
  | V.length xs < 2 = xs
  | otherwise
    = let h = V.head xs
          small = V.filter (< h) xs
          mid   = V.filter (== h) xs
          large = V.filter (> h) xs
       in (qsort small) V.++ mid V.++ (qsort large)


-- Quick sort parallel; uses the same function for parallel merging as merge sort
sortSkelQ :: V.Vector Int -> V.Vector Int
sortSkelQ = fixedDivideAndConquer parallelWorkLoad merge qsort


-- Quick sort parallel and more general
qsortSkel :: V.Vector Int -> V.Vector Int
qsortSkel = fixedDivideAndConquer' parallelWorkLoad merge (V.++) q_partition (\x -> V.length x < 2) id
  where
    q_partition xs
      = let h = V.head xs
            small = V.filter (< h) xs
            mid   = V.filter (== h) xs
            large = V.filter (> h) xs
         in V.fromList [small, mid, large]


-- A more pragmatic quick sort
-- qsort :: (Ord a) => V.Vector a -> V.Vector a
-- qsort = V.modify go where
--     go xs | M.length xs < 2 = return ()
--           | otherwise = do
--             p <- M.read xs (M.length xs `div` 2)
--             j <- M.unstablePartition (< p) xs
--             let (l, pr) = M.splitAt j xs
--             k <- M.unstablePartition (== p) pr
--             go l;
--             go $ M.drop k pr

