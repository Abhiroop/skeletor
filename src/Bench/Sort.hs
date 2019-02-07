{-# LANGUAGE RankNTypes #-}
module Bench.Sort where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as M
import Data.List
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
  let vec = qsortSkel' (V.fromList l)
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
sortSkel' = fixedDivideAndConquer' 10 merge merge (\x -> V.splitAt (V.length x `div` 2) x) (\x -> length x < 2) id

qsortSkel' :: V.Vector Int -> V.Vector Int
qsortSkel' = fixedDivideAndConquer' 10 merge (V.++) q_partition (\x -> length x < 2) id
  where
    q_partition xs
      = let h = V.head xs
            small = V.filter (< h) xs
            large = V.filter (> h) xs
         in (small, large)


--------------------------------------------------


qsort :: (Ord a) => V.Vector a -> V.Vector a
qsort = V.modify go where
    go xs | M.length xs < 2 = return ()
          | otherwise = do
            p <- M.read xs (M.length xs `div` 2)
            j <- M.unstablePartition (< p) xs
            let (l, pr) = M.splitAt j xs
            k <- M.unstablePartition (== p) pr
            go l;
            go $ M.drop k pr

