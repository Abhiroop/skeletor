module Control.Parallel where

import Control.Monad (join)
import qualified Control.Monad.Par as MP
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Data.List
import Data.Maybe

type K = Int

class Parallelizable t where
  parMap :: (MP.NFData b) => K -> (a -> b) -> t a -> t b
  parSplit :: K -> t a -> t (t a)
  parJoin :: t (t a) -> t a

instance Parallelizable V.Vector where
  parMap k f = parJoin . MP.runPar . MP.parMap (fmap f) . parSplit k
  parSplit k vec = V.fromList $ go 0 [] -- Addition O(n) for fromList
    where
      go i xs
        | (i + k) < V.length vec =
          let v = V.unsafeSlice i k vec
          in v : go (i + k) xs
        | otherwise = V.unsafeSlice i (V.length vec - i) vec : xs
  parJoin = join -- concat for vector is involved
 -- new thread -> map f [1..n] and return result
-- newtype Set =
--   S [Set]
--   deriving (Eq)
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
--     e (Singleton s) = e s
--     e (Union s1 s2) = S $ unset (e s1) `union` unset (e s2)
--     e (Intersection s1 s2) = S $ unset (e s1) `intersect` unset (e s2)
--     e (Var v) =
--       fromMaybe (S []) $
--       fmap snd $
--       find
--         (\(x, s) ->
--            if x == v
--              then True
--              else False)
--         env
