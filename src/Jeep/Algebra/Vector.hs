module Jeep.Algebra.Vector where

-----------------------------------

import Data.Semiring as SR

-----------------------------------

type Vector s = [s]
type Matrix s = [Vector s]

-----------------

vmul :: SR.Semiring s => Vector s -> Vector s -> s
vmul xs = foldr (SR.+) zero . zipWith (SR.*) xs

transpose :: Matrix s -> Matrix s
transpose [] = []
transpose ([] : xss) = transpose xss
transpose xss = (map head xss) : transpose (map tail xss)

-- mscale :: Scalar s => s -> Matrix s -> Matrix s
-- mscale c = map (map (c*.))

mmul :: SR.Semiring s => Matrix s -> Matrix s -> Matrix s
mmul xss yss = go xss where
  cols = transpose yss
  go [] = []
  go (r:rs) = map (vmul r) cols:go rs

mvmul :: SR.Semiring s => Matrix s -> Vector s -> Vector s
mvmul xss ys =
  head $ transpose (mmul xss (transpose [ys]))

vmmul :: SR.Semiring s => Vector s -> Matrix s -> Vector s
vmmul ys xss =
  head $ mmul [ys] xss

-- End ---------------------------------------------------------------
