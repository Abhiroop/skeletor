module Control.Parallel where

import Control.Monad (void)
import Data.Concurrent.Deque.Reference
import qualified Data.Vector as V
import GHC.Conc
import System.IO.Unsafe
import Debug.Trace

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
            tids <- traverse threadStatus threads
            if any (/= ThreadFinished) tids
            then go i dq threads
            ------------------------
            else do {
                    pushR q (f (V.unsafeSlice i (V.length vec - i) vec));
                    mapM_ killThread threads; -- be a good citizen! clean up resources! XXX: This is an O(n) operation could it slow us down?
                    return q
                    }


  -- XXX: getNumCapablities should be used instead of just spawning new threads??
  parJoin merge de_q = unsafePerformIO (finalElem de_q []) -- don't try this at home
    where
      finalElem dq threads = do
        q <- dq
        tids <- traverse threadStatus threads -- O(n) traversal everytime could we reduce it?
        x <- tryPopL q
        case x of
          Nothing -> do {
                        if all (== ThreadFinished) tids
                        then return V.empty
                        else finalElem (return q) threads
                        }
          Just e1 -> do
            y <- tryPopL q
            case y of
              Nothing -> do {
                            if all (== ThreadFinished) tids
                            then do { mapM_ killThread threads; return e1 } -- be a good citizen! resource cleanup
                            else finalElem (do {pushR q e1; return q}) threads -- XXX: V.V Subtle: popped an element and found that there are threads running;
                                                                               --                  put the element back and spin
                            }
              Just e2 -> do
                tid <- forkIO $ pushR q (merge e1 e2) -- cpu intensive could be vectorized as well additionally merge for vector will be costly
                finalElem (return q) (tid:threads)
