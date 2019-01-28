module Control.Cluster where

import Data.Vector

import Prelude hiding (head)

-- this is a variant of iterative combine
type Depth = Int

cluster ::
     (Depth -> obj -> obj -> Bool) -- match
  -> (Depth -> obj -> obj -> obj) -- reshape
  -> (Depth -> obj -> Bool) -- continue
  -> Vector obj
  -> Vector obj
cluster match reshape continue = go 0
  where
    go d set
      | continue (d + 1) (head set) =
        amend d . union . fmap (go (d + 1)) . decompose d $ set
      | otherwise = amend d set

    -- decompose first forms all the possible pairs and then uses `match`
    -- to collect together the similar elements
    decompose :: Depth -> Vector obj -> Vector (Vector obj)
    decompose = undefined

    -- amend uses reshape to every pair of elements in the cluster
    amend :: Depth -> Vector obj -> Vector obj
    amend = undefined

    union :: Vector (Vector obj) -> Vector obj
    union = undefined
