module Bench.SortSeq where

import qualified Data.Sequence as S
import Data.List
import Control.DivideAndConquer
import System.Random (randomRIO)

randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,6)
  rs <- randomList (n-1)
  return (r:rs)

bench2 :: IO ()
bench2 = do
  l <- randomList 10000000
  print $ "Starting sort for list of length " ++ show (length l)
  let -- vec   = mergesort (S.fromList l)
      vec   = sortSkelM (S.fromList l)
      -- vec   = S.sort (S.fromList l)
  --print vec
  print $ S.lookup 9999999 vec


parallelWorkLoad :: Int
parallelWorkLoad = 100000
-------------------------------------------------------------------------------------
-- Merge sort

merge :: S.Seq Int -> S.Seq Int -> S.Seq Int
merge xs ys
  | S.length xs == 0 = ys
  | S.length ys == 0 = xs
  | otherwise
    = let (h1,t1) = S.splitAt 1 xs
          (h2,t2) = S.splitAt 1 ys
       in if h1 < h2
          then h1 S.>< (merge t1 ys)
          else h2 S.>< (merge xs t2)

-- Merge sort sequential
mergesort :: S.Seq Int -> S.Seq Int
mergesort xs
  | S.length xs < 2 = xs
  | otherwise = let (first,second) = S.splitAt half xs
                in merge (mergesort first) (mergesort second)
  where
    half = S.length xs `div` 2

-- Merge sort parallel

{-# SPECIALISE fixedDivideAndConquer' :: Int
                      -> (S.Seq Int -> S.Seq Int -> S.Seq Int)
                      -> (S.Seq Int -> S.Seq Int)
                      -> S.Seq Int
                      -> S.Seq Int
 #-}
sortSkelM :: S.Seq Int -> S.Seq Int
sortSkelM = fixedDivideAndConquer' parallelWorkLoad merge S.sort

{-

{-# INLINE[1] id' #-}
id' :: a -> a
id' x = x

{-# RULES
  "fmap/id"    forall xs (id' :: a -> a).  fmap id' xs = xs
    #-}

-}

-- specialising perhaps saves the dictionary lookup but isn't a huge optimisation
-- Merge sort parallel and more general
{-# SPECIALISE fixedDivideAndConquer :: Int
                      -> (S.Seq Int -> S.Seq Int -> S.Seq Int)
                      -> (S.Seq Int -> S.Seq Int -> S.Seq Int)
                      -> (S.Seq Int -> [(S.Seq Int)])
                      -> (S.Seq Int -> Bool)
                      -> (Int -> Int)
                      -> S.Seq Int
                      -> S.Seq Int
 #-}
msortSkel :: S.Seq Int -> S.Seq Int
msortSkel = fixedDivideAndConquer parallelWorkLoad merge merge m_partition (\x -> S.length x < 2) id
  where
    m_partition xs
      = let (first, second) = S.splitAt (S.length xs `div` 2) xs
         in [first,second]

--------------------------------------------------------------------------
-- Quick sort

-- Quick sort sequential
-- qsort :: V.Vector Int -> V.Vector Int
-- qsort xs
--   | V.length xs < 2 = xs
--   | otherwise
--     = let h = V.head xs
--           small = V.filter (< h) xs
--           mid   = V.filter (== h) xs
--           large = V.filter (> h) xs
--        in (qsort small) V.++ mid V.++ (qsort large)


-- Quick sort parallel and more general
-- qsortSkel :: V.Vector Int -> V.Vector Int
-- qsortSkel = fixedDivideAndConquer parallelWorkLoad merge (V.++) q_partition (\x -> V.length x < 2) id
--   where
--     q_partition xs
--       = let h = V.head xs
--             small = V.filter (< h) xs
--             mid   = V.filter (== h) xs
--             large = V.filter (> h) xs
--          in [small, mid, large]


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

