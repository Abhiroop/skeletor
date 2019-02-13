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
  l <- randomList 100
  print $ "Starting sort for list of length " ++ show (length l)
  let vec = sortSkel' (V.fromList l)
  print vec
  print $ V.length vec

type N = Int

mergesort :: V.Vector Int -> V.Vector Int
mergesort xs
  | V.length xs < 2 = xs
  | otherwise = let (first,second) = split half xs
                in merge (mergesort first) (mergesort second)
  where
    half = V.length xs `div` 2

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

split :: N -> V.Vector Int -> (V.Vector Int, V.Vector Int)
split = V.splitAt

sortSkel :: V.Vector Int -> V.Vector Int
sortSkel = fixedDivideAndConquer 1000 merge mergesort

sortSkel' :: V.Vector Int -> V.Vector Int
sortSkel' = fixedDivideAndConquer'' 10 merge merge m_partition (\x -> V.length x < 2) id
  where
    m_partition xs
      = let (first, second) = V.splitAt (V.length xs `div` 2) xs
         in V.fromList [first,second]

qsortSkel' :: V.Vector Int -> V.Vector Int
qsortSkel' = fixedDivideAndConquer'' 10 merge (V.++) q_partition (\x -> V.length x < 2) id
  where
    q_partition xs
      = let h = V.head xs
            small = V.filter (< h) xs
            mid   = V.filter (== h) xs
            large = V.filter (> h) xs
         in V.fromList [small, mid, large]

--qsortSkel = fixedDivideAndConquer 10 merge qsort

qsort' :: V.Vector Int -> V.Vector Int
qsort' xs
  | V.length xs < 2 = xs
  | otherwise
    = let h = V.head xs
          small = V.filter (< h) xs
          mid   = V.filter (== h) xs
          large = V.filter (> h) xs
       in (qsort' small) V.++ mid V.++ (qsort' large)

q_partition xs
  = let h = V.head xs
        small = V.filter (< h) xs
        mid   = V.filter (== h) xs
        large = V.filter (> h) xs
     in V.fromList [small, mid, large]

func :: V.Vector Int -> V.Vector Int
func ta
  | (\x -> V.length x < 2) ta = trace ("abhi" ++ show ta) id ta
  | otherwise   = let tta = q_partition ta
                   in foldl' (\u ta' -> if ta' == ta then u V.++ id ta' else u V.++ (func ta')) mempty tta

--------------------------------------------------


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

-- qsort xs
--   | V.length xs == 0 = V.empty
--   | otherwise = qsort small V.++ qsort large
--   where
--     small = V.filter (<= x) xs
--     large = V.filter (> x) xs
--     x     = V.head xs

