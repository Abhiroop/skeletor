module Control.Parallel where

import Control.Monad (join, void)
import Data.Concurrent.Deque.Reference
import qualified Data.Vector as V
import GHC.Conc
import System.IO.Unsafe

import Data.List
import Data.Maybe

type K = Int

class Parallelizable t where
  parSplit ::
       K -- partition size
    -> (t a -> t b)
    -> t a
    -> IO (SimpleDeque (t b))
  parJoin ::
       (t b -> t b -> t b) -- how do you join?
    -> IO (SimpleDeque (t b))
    -> t b

instance Parallelizable V.Vector where
  parSplit k f vec = go 0 newQ
    where
      go i dq
        | (i + k) < V.length vec = go (i + k) $ modifyDQ (V.unsafeSlice i k vec)
        | otherwise = modifyDQ (V.unsafeSlice i (V.length vec - i) vec)
        where
          modifyDQ x = do { q <- dq ; void $ forkIO $ pushR q (f x) ; return q }
  -- XXX: getNumCapablities should be used instead of just forking threads
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
                                                                         -- additionally merge for vector will be costly
                                   finalq
                      in unsafePerformIO $
                         do {q <- finalq ;
                             e <- tryPopL q ;
                             maybe (return V.empty) return e}
