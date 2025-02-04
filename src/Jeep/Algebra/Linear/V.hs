module Jeep.Algebra.Linear.V where

-----------------------------------

import qualified Data.Semiring as SR

-----------------------------------

type V s = [s]
type M s = V (V s)

-----------------

vmul :: SR.Semiring s => V s -> V s -> s
vmul xs = foldr (SR.plus) SR.zero . zipWith (SR.*) xs

transpose :: M s -> M s
transpose [] = []
transpose ([] : xss) = transpose xss
transpose xss = (map head xss) : transpose (map tail xss)

mscale :: SR.Semiring s => s -> M s -> M s
mscale c = map (map ((SR.times) c))

mmul :: SR.Semiring s => M s -> M s -> M s
mmul xss yss = go xss where
  cols = transpose yss
  go [] = []
  go (r:rs) = map (vmul r) cols:go rs

mvmul :: SR.Semiring s => M s -> V s -> V s
mvmul xss ys =
  head $ transpose (mmul xss (transpose [ys]))

vmmul :: SR.Semiring s => V s -> M s -> V s
vmmul ys xss = head $ mmul [ys] xss

-- End ---------------------------------------------------------------
