module Jeep.Algebra.Linear.Dense.M where

-----------------------------------

import qualified Data.Semiring as SR

import Jeep.Data.List
import Jeep.Algebra.Linear.Dense.V

-----------------------------------

type M s = V (V s)

-----------------------------------

mtabulate :: Int -> Int -> (Int -> Int -> a) -> M a
mtabulate r c f = [ [ f i j | i <- [0..c-1]] | j <- [0..r-1]]

midentity :: (Eq s, SR.Semiring s) => Int -> M s
midentity n = mtabulate n n kr where
  kr i j = if i == j then SR.one else SR.zero

mconst :: Int -> Int -> s -> M s
mconst r c k = mtabulate r c (\_ _ -> k)

mzeroes :: (SR.Semiring s) => Int -> Int -> M s
mzeroes r c = mconst r c SR.zero

mones :: (SR.Semiring s) => Int -> Int -> M s
mones r c = mconst r c SR.one

-----------------------------------

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

mcirculant :: V a -> M a
mcirculant v = (\i -> rotate (-i) v)  <$> [0 .. (n-1)] where
  n = length v

msquare :: [[a]] -> Bool
msquare [] = True
msquare xss = all (\xs -> length xs == length (head xss)) xss

-- End ---------------------------------------------------------------
