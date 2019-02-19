{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Matrix where

import GHC.TypeLits

import Data.Proxy (Proxy(..))
import qualified Data.Vector as V


data Tree a = Leaf
            | Node a (Tree a) (Tree a)

data Matrix a = Mat2X2 (Matrix a) (Matrix a) (Matrix a) (Matrix a)
              | Mat1X2 (Matrix a) (Matrix a)
              | Mat2X1 (Matrix a) (Matrix a)
              | Elem a
              | Nil

foo = Mat2X2 (Elem 1) (Elem 2) (Elem 3) (Elem 4)

bar = Mat2X2 (Elem 5) (Elem 6) (Elem 7) (Elem 8)

-- mult :: Matrix a -> Matrix a -> Matrix a
-- mult (Mat e1 e2 e3 e4) (Mat e5 e6 e7 e8)
--   = Mat (e1*e5 + e2*e7) 0 0 0

-- data Matrix (r :: Nat) (c :: Nat) (e :: *) = Matrix { rows :: !Int
--                                                     , cols :: !Int
--                                                     , elems :: V.Vector e } -- some strucuture which has efficient slice and concat

-- nrows :: forall m n a. KnownNat m => Matrix m n a -> Int
-- nrows = const m
--   where m = fromInteger $ natVal @m Proxy

-- ncols :: forall m n a. KnownNat n => Matrix m n a -> Int
-- ncols = const n
--   where n = fromInteger $ natVal @n Proxy


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
