module Bench.Sort where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
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
  print $ "Starting sort for list of length " ++ (show $ length l)
  let vec = sortSkel (V.fromList l)
  print $ vec
  print $ V.length vec

type N = Int

sortV :: V.Vector Int -> V.Vector Int
sortV xs
  | V.length xs < 2 = xs
  | otherwise = let (first,second) = split half xs
                in merge (sortV first) (sortV second)
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
sortSkel = fixedDivideAndConquer 10 merge sortV
