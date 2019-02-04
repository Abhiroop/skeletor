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

type N = Int

sortV :: Vector Int -> Vector Int
sortV xs
  | Data.Vector.length xs < 2 = xs
  | otherwise = let (first,second) = split half xs
                in merge (sortV first) (sortV second)
  where
    half = Data.Vector.length xs `div` 2

merge :: Vector Int -> Vector Int -> Vector Int
merge = undefined

split :: N -> Vector Int -> (Vector Int, Vector Int)
split = undefined

bar :: Vector Int -> Vector Int
bar = fixedDivideAndConquer 10 merge sortV
