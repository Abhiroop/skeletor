module Control.IterativeCombine where

import Data.Vector

import Prelude hiding (foldr)

import Data.List hiding (foldr)

combinations 0 lst = [[]]
combinations n lst = do
    (x:xs) <- tails lst
    rest   <- combinations (n-1) xs
    return $ x : rest

iterativeCombine :: (Eq obj, Eq val)
                 => (obj -> obj -> obj)  -- combine
                 -> (obj -> obj -> val)  -- value
                 -> (val -> val -> Bool) -- accept
                 -> Vector obj
                 -> Vector obj
iterativeCombine combine value accept = func
  where func s
          | continue s = func (merge (partners s) s)
          | otherwise  = s
            where
              -- parallelizable
              merge :: Vector (obj,obj) -> Vector obj -> Vector obj
              merge = undefined

              {-
              partners initially generates all the combinations eg: combinations of [1,2,3,4]
              [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]] now for each element say 1 calculate `value`
              compare all the `values` to take the best and return that for each element eg:
              partner will return the best like [(1,2),(2,3),(3,2)(4,2)]

              merge will compare the best among them and merge all those possible. For eg in this case
              only one can be merged. and return the rest so imaging (2,3) is the best so merge will
              return [1, combine 2 3, 4 5] then continue will test and keep iterating
              -}
              {-
              this is a very heavy computation and the best candidate for parallelism.
              Recipe:
              split the the first vector into parts in O(1)
              now for each vector take the desired combination and calculate the `value`
              when you have an exhaustive list of Vector(obj,obj) find the best for each element possibly
              sequentially or using a vectorized instruction
              If there is a spark provided for each element now combine all those sparks in parallel
              -}
              partners :: Vector obj -> Vector (obj,obj)
              partners = undefined


              continue xs = merge (partners xs) xs /= xs -- if fixed point not reached then continue else stop



