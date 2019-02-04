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










-- newtype Set =
--   S [Set]
--   deriving (Eq, Show, Ord)
-- unset :: Set -> [Set]
-- unset (S xs) = xs
-- data SET v
--   = Empty
--   | Singleton (SET v)
--   | Union (SET v)
--           (SET v)
--   | Intersection (SET v)
--                  (SET v)
--   | Var v
-- type Env var dom = [(var, dom)]
-- eval :: Eq v => Env v Set -> SET v -> Set
-- eval env = e
--   where
--     e Empty = S []
--     e (Singleton s) = S $ [e s]
--     e (Union s1 s2) = S $ unset (e s1) `union` unset (e s2)
--     e (Intersection s1 s2) = S $ filter (`elem` (unset $ e s2)) (unset $ e s1)
--     e (Var v) =
--       fromMaybe (S []) $
--       fmap snd $
--       find
--         (\(x, s) ->
--            if x == v
--              then True
--              else False)
--         env
-- vN :: (Eq t, Num t) => t -> SET v
-- vN 0 = Empty
-- vN n = Union (vN $ n - 1) (Singleton (vN $ n - 1))
-- vonNeumann :: Int -> Set
-- vonNeumann 0 = S []
-- vonNeumann n = S $ last:(unset last)
--                where
--                  last = vonNeumann $ n - 1
-- suc :: Set -> Set
-- suc it = S (it : unset it)
-- nats :: [Set]
-- nats = (S []) : (map suc nats)
