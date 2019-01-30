module Control.Parallel where

import Data.Vector

type K = Int

class Parallelizable t where
  parMap :: (prob -> sol) -> t prob -> t sol
  split  :: prob  -> t prob
  join   :: t prob -> prob

instance Parallelizable Vector where
  parMap = undefined

  split  = undefined
  join   = undefined
