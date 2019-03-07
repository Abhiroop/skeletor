module MMult where

import Data.Matrix

matmult :: Matrix a -> Matrix a -> Matrix a
matmult m1 m2 = undefined
  -- let c1    = cols m -- gives all the columns
  --     (h1,t1) = slice m (c1/2) -- (c/2) is some kind of type level division
  --     rm1    = rows h1
  --     rm2    = rows t1
  --     (p1, p3) = partition (rm1/2) h1
  --     (p2, p4) = partition (rm2/2) t1

  --     c2     = cols m -- gives all the columns
  --     (h2,t2) = slice m (c2/2) -- (c/2) is some kind of type level division
  --     rn1    = rows h2
  --     rn2    = rows t2
  --     (p5, p7) = partition (r/2) h2
  --     (p6, p8) = partition (r/2) t2



  --     a = p1*p5 + p2*p7
  --     b = p1*p6 + p2*p8
  --     c = p3*p5 + p4*p7
  --     d = p3*p6 + p4*p8
  --   in (copartition a b) `coslice` (copartition c d)
