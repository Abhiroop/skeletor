module Bench.MMult where

import Data.Matrix
import qualified Data.Vector as V
import Control.DivideAndConquer
import Control.Applicative

mulThreshold = 64  -- 64 * 64 * 8 = 32768 L1 cache size
addThreshold = 2000 -- 2000 * 2000 = 4 million elements single threaded

(%*%) :: (Num a) => Matrix a -> Matrix a -> Matrix a
(%*%) m1 m2
  | nrows m1 <= mulThreshold && ncols m2 <= mulThreshold = multStrassen m1 m2
  | otherwise
    = let r = nrows m1
          c = ncols m2
          (p1, p2, p3, p4) = splitBlocks (r `div` 2) (c `div` 2) m1
          (p5, p6, p7, p8) = splitBlocks (r `div` 2) (c `div` 2) m2
          x = (p1 %*% p5) %+% (p2 %*% p7)
          y = (p1 %*% p6) %+% (p2 %*% p8)
          z = (p3 %*% p5) %+% (p4 %*% p7)
          w = (p3 %*% p6) %+% (p4 %*% p8)
      in joinBlocks (x, y, z, w)

(%+%) :: (Num a) => Matrix a -> Matrix a -> Matrix a
(%+%) m1 m2
  | nrows m1 <= addThreshold && ncols m2 <= addThreshold
    = undefined
  | otherwise
    = let r = nrows m1
          c = ncols m2
          (p1, p2, p3, p4) = splitBlocks (r `div` 2) (c `div` 2) m1
          (p5, p6, p7, p8) = splitBlocks (r `div` 2) (c `div` 2) m2
          x = (p1 %+% p5)
          y = (p2 %+% p6)
          z = (p3 %+% p7)
          w = (p4 %+% p8)
      in joinBlocks (x, y, z, w)

