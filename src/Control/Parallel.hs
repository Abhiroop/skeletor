module Control.Parallel where

import Control.Monad (join, void)
import qualified Control.Monad.Par as MP
import Data.Concurrent.Deque.Reference
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Conc
import System.IO.Unsafe

import Data.List
import Data.Maybe

type K = Int

class Parallelizable t where
  parApply ::
       (MP.NFData b)
    => K -- for forking
    -> (t a -> t a -> t a) -- for joining
    -> (t a -> t b) -- sequential operation might not be necessary
    -> t a
    -> t b
  parSplit ::
       K -- partition size
    -> t a
    -> IO (SimpleDeque (t a))
  parJoin ::
       (t a -> t a -> t a) -- how do you join?
    -> IO (SimpleDeque (t a))
    -> t a

instance Parallelizable V.Vector where
  parApply k merge f = undefined --parJoin merge . MP.runPar . MP.parMap f . parSplit k
  parSplit k vec = go 0 newQ
    where
      go i dq
        | (i + k) < V.length vec = go (i + k) $ modifyDQ (V.unsafeSlice i k vec)
        | otherwise = modifyDQ $ V.unsafeSlice i (V.length vec - i) vec
        where
          modifyDQ x = do { q <- dq ; pushR q x ; return q }
  parJoin merge dq = let finalq = do
                           q <- dq
                           x <- tryPopL q
                           case x of
                             Nothing -> return q
                             Just e1 -> do
                               y <- tryPopL q
                               case y of
                                 Nothing -> return q
                                 Just e2 -> do
                                   void $ forkIO $ pushR q (merge e1 e2) -- cpu intensive could be vectorized as well
                                   finalq
                      in unsafePerformIO $
                         do {q <- finalq ;
                             e <- tryPopL q ;
                             maybe (return V.empty) return e}










