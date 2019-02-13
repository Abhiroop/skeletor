module Utils where

import Control.Concurrent.Async
import GHC.Conc

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)


killThreads :: Foldable t => t ThreadId -> IO ()
killThreads = mapM_ killThread

killAsyncs :: (Functor t, Foldable t) => t (Async a) -> IO ()
killAsyncs = mapM_ cancel
