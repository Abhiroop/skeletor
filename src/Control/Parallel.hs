module Control.Parallel where

import Data.Concurrent.Deque.Reference
import GHC.Conc
import System.IO.Unsafe (unsafePerformIO)
import Utils

import qualified Data.Vector as V
import qualified Data.Sequence as S

type K = Int

class Parallelizable t where
  parSplit ::
       K -- partition size
    -> (t a -> t b)
    -> t a
    -> IO (SimpleDeque (t b))
  parSplit2 ::
       K -- partition size
    -> (t a -> t b -> t c)
    -> t a
    -> t b
    -> IO (SimpleDeque (t c))
  parJoin ::
       (t b -> t b -> t b) -- how do you join?
    -> IO (SimpleDeque (t b))
    -> t b


instance Parallelizable V.Vector where
  {-# INLINE parSplit #-}
  parSplit k f vec = go 0 newQ []
    where
      go i dq threads
        | (i + k) < V.length vec = do
            q   <- dq;
            tid <- forkIO $ pushR q (f (V.unsafeSlice i k vec));
            go (i + k) (return q) (tid :threads)
        | otherwise = do
            q <- dq;
            --- a cyclic barrier ---
            tStatus <- traverse threadStatus threads
            if any (/= ThreadFinished) tStatus
            then go i dq threads
            ------------------------
            else do {
                    pushR q (f (V.unsafeSlice i (V.length vec - i) vec));
                    killThreads threads; -- be a good citizen! clean up resources! XXX: This is an O(n) operation could it slow us down?
                    return q
                    }

  -- XXX: getNumCapablities should be used instead of just spawning new threads??
  {-# INLINE parJoin #-}
  parJoin merge de_q = unsafePerformIO (finalElem de_q []) -- don't try this at home
    where
      finalElem dq threads = do
        q <- dq
        tids <- traverse threadStatus threads -- O(n) traversal everytime could we reduce it?
        x <- tryPopL q
        case x of
          Nothing -> do {
                        if all (== ThreadFinished) tids
                        then do {killThreads threads; return V.empty}
                        else finalElem (return q) threads
                        }
          Just e1 -> do
            y <- tryPopL q
            case y of
              Nothing -> do {
                            if all (== ThreadFinished) tids
                            then do { killThreads threads; return e1 } -- be a good citizen! resource cleanup
                            else finalElem (do {pushR q e1; return q}) threads -- XXX: V.V Subtle: popped an element and found that there are threads running;
                                                                               --                  put the element back and spin
                            }
              Just e2 -> do
                tid <- forkIO $ pushR q (merge e1 e2) -- cpu intensive could be vectorized as well additionally merge for vector will be costly
                finalElem (return q) (tid:threads)

  {-# INLINE parSplit2 #-}
  parSplit2 k f vec1 vec2 = go 0 newQ []
    where
      go i dq threads
        | ((i + k) < V.length vec1) && ((i + k) < V.length vec2)= do
            q   <- dq;
            tid <- forkIO $ pushR q (f (V.unsafeSlice i k vec1) (V.unsafeSlice i k vec2));
            go (i + k) (return q) (tid :threads)
        | otherwise = do
            q <- dq;
            --- a cyclic barrier ---
            tStatus <- traverse threadStatus threads
            if any (/= ThreadFinished) tStatus
            then go i dq threads
            ------------------------
            else do {
                    pushR q (f (V.unsafeSlice i (V.length vec1 - i) vec1) (V.unsafeSlice i (V.length vec2 - i) vec2));
                    killThreads threads; -- be a good citizen! clean up resources! XXX: This is an O(n) operation could it slow us down?
                    return q
                    }



--------------------------------------------------------
-- Simpler thread management strategy seems to be faster




-- XXX: getNumCapablities should be used instead of just spawning new threads??

  -- parJoin merge de_q = unsafePerformIO (finalElem de_q []) -- don't try this at home
  --   where
  --     finalElem dq threads = do
  --       q <- dq
  --       (idleThreads, activeThreads) <- partitionM (\tid -> do {
  --                                                     tStatus <- threadStatus tid;
  --                                                     return $ tStatus == ThreadFinished}) threads
  --       x <- tryPopL q
  --       case x of
  --         Nothing -> if null activeThreads
  --                    then do { killThreads idleThreads; return V.empty }
  --                    else do { killThreads idleThreads; finalElem (return q) activeThreads }
  --         Just e1 -> do
  --           y <- tryPopL q
  --           case y of
  --             Nothing -> if null activeThreads
  --                        then do { killThreads idleThreads; return e1 } -- be a good citizen! resource cleanup
  --                        else do { killThreads idleThreads;
  --                                  finalElem (do {pushR q e1; return q}) activeThreads} -- XXX: Subtle: popped an element and found that there are threads running;
  --                                                                                       --              put the element back and spin
  --             Just e2 -> do
  --               tid <- forkIO $ pushR q (merge e1 e2) -- cpu intensive could be vectorized as well additionally merge for vector will be1 costly
  --               finalElem (return q) (tid:threads)

{- Alternative more efficient threading design:
We need something called `partitionM`

partitionM :: (Monad m ) => (a -> m Bool) -> [a] -> ([a], [a])

let (deadThreads, liveThreads) = partition (== ThreadFinished) tStatus
if length liveThreads == 0
then kill all dead threads and return
else do {kill dead threads; recurse with the live threads}


-}
instance Parallelizable S.Seq where
  {-# INLINE parSplit #-}
  parSplit k f s = go s newQ []
    where
      go s' dq threads
        | S.length s' == 0 = do
            q <- dq;
            --- a cyclic barrier ---
            tStatus <- traverse threadStatus threads
            if any (/= ThreadFinished) tStatus
            then go s' dq threads
            ------------------------
            else do { killThreads threads; return q}

        | otherwise = do
            q   <- dq;
            let (s1, s2) = S.splitAt k s'
            tid <- forkIO $ pushR q (f s1);
            go s2 (return q) (tid :threads)

  {-# INLINE parJoin #-}
  parJoin merge de_q = unsafePerformIO (finalElem de_q [])
    where
      finalElem dq threads = do
        q <- dq
        tids <- traverse threadStatus threads
        x <- tryPopL q
        case x of
          Nothing -> do {
                        if all (== ThreadFinished) tids
                        then do {killThreads threads; return S.empty}
                        else finalElem (return q) threads
                        }
          Just e1 -> do
            y <- tryPopL q
            case y of
              Nothing -> do {
                            if all (== ThreadFinished) tids
                            then do { killThreads threads; return e1 }
                            else finalElem (do {pushR q e1; return q}) threads
                            }
              Just e2 -> do
                tid <- forkIO $ pushR q (merge e1 e2)
                finalElem (return q) (tid:threads)
