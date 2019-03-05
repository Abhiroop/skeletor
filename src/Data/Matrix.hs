{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Matrix where

import qualified Data.Vector as V

data Nat = Z
         | Succ Nat
         deriving Show

type One = Succ Z
type Two = Succ One


toNat :: Int -> Nat
toNat 0 = Z
toNat n = Succ (toNat $ n - 1)

fromNat :: Nat -> Int
fromNat Z = 0
fromNat (Succ n) = 1 + fromNat n

-- fromNat . toNat = id

type family Plus a b where
  Plus Z     n    = n
  Plus (Succ m) n = Succ (Plus m n)

data Matrix r c a where
  B :: Matrix r c1 a -> Matrix r c2 a -> Matrix r (Plus c1 c2) a
  A :: Matrix r1 c a -> Matrix r2 c a -> Matrix (Plus r1 r2) c a
  S :: a -> Matrix One One a

foo = A (B (S 1) (S 2)) (B (S 3) (S 4))
bar = A (B (S 5) (S 6)) (B (S 7) (S 8))
baz = B (B (A (S 1) (S 2)) (A (S 3) (S 4))) (A (S 5) (S 6))


type ColIndex = Int
type RowIndex = Int

slice :: (Plus c1 c2 ~ c) => Matrix r c e ->  ColIndex -> (Matrix r c1 e, Matrix r c2 e)
slice = undefined

partition :: (Plus r1 r2 ~ r) => Matrix r c e ->  RowIndex -> (Matrix r1 c e, Matrix r2 c e)
partition = undefined




-- the dimensions stuff seems to complicate the implementation rather than simplify it

--some vocabulary to talk about index
-- somthing optimized for slicing and partition

-- matmult m1 m2 =
--   let c1    = cols m -- gives all the columns
--       (h1,t1) = slice m (c1/2) -- (c/2) is some kind of type level division
--       rm1    = rows h1
--       rm2    = rows t1
--       (p1, p3) = partition (rm1/2) h1
--       (p2, p4) = partition (rm2/2) t1

--       c2     = cols m -- gives all the columns
--       (h2,t2) = slice m (c2/2) -- (c/2) is some kind of type level division
--       rn1    = rows h2
--       rn2    = rows t2
--       (p5, p7) = partition (r/2) h2
--       (p6, p8) = partition (r/2) t2



--       a = p1*p5 + p2*p7
--       b = p1*p6 + p2*p8
--       c = p3*p5 + p4*p7
--       d = p3*p6 + p4*p8
--     in (copartition a b) `coslice` (copartition c d)

-- type ColIndex = Nat
-- type RowIndex = Nat

-- slice     :: Matrix r c e ->  ColIndex -> (Matrix r c1 e, Matrix r c2 e | c1 + c2 = c)

-- partition :: Matrix r c e ->  RowIndex -> (Matrix r1 c e, Matrix r2 c e | r1 + r2 = r)

-- coslice :: Matrix r c1 e -> Matrix r c2 e -> (Matrix r c e | c = c1 + c2)

-- copartition :: Matrix r1 c e -> Matrix r2 c e -> (Matrix r c e | r = r1 + r2)
