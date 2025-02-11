module Jeep.Algebra.Linear.M where

-----------------------------------

import qualified Data.Semiring as SR

import Jeep.Data.List
import Jeep.Algebra.Linear.V

-----------------------------------

type M s = V (V s)

-----------------------------------

identity :: (Eq s, SR.Semiring s) => Int -> M s
identity n = [ [ kr i j | i <- [0..n-1]] | j <- [0..n-1]] where
  kr i j = if i == j then SR.one else SR.zero

mtranspose :: M s -> M s
mtranspose [] = []
mtranspose ([] : xss) = mtranspose xss
mtranspose xss = map head xss : mtranspose (map tail xss)

mscale :: SR.Semiring s => s -> M s -> M s
mscale c = map (map (SR.times c))

mmul :: SR.Semiring s => M s -> M s -> M s
mmul xss yss = go xss where
  cols = mtranspose yss
  go [] = []
  go (r:rs) = map (vmul r) cols:go rs

mvmul :: SR.Semiring s => M s -> V s -> V s
mvmul xss ys =
  head $ mtranspose (mmul xss (mtranspose [ys]))

vmmul :: SR.Semiring s => V s -> M s -> V s
vmmul ys xss = head $ mmul [ys] xss

circulant :: V a -> M a
circulant v = (\i -> rotate (-i) v)  <$> [0 .. (n-1)] where
  n = length v
 
-- End ---------------------------------------------------------------
