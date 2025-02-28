module Jeep.Algebra.Linear.Sparse.V where

-----------------------------------

import Control.Exception

import qualified Data.Map as M

-----------------------------------

import qualified Data.Semiring as SR

-----------------------------------

type V ix s = M.Map ix s

-----------------

vtabulate :: Ord ix => [ix] -> (ix -> a) -> V ix a
vtabulate ixs f = M.fromList $ (\i -> (i,f i)) <$> ixs

vsparse :: Ord ix => [ix] -> [a] -> V ix a
vsparse indices xs =
  assert (length indices == length xs) 
  M.fromList (zip indices xs)

vdense :: V ix s -> [s]
vdense m = snd <$> M.toList m

vcat :: Ord ix => V ix s -> V ix s -> V ix s
vcat = M.union
-- ^ https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Lazy.html#v:union
-- The expression (union t1 t2) takes the left-biased union of t1 and t2. 

vscale :: SR.Semiring s => s -> V ix s -> V ix s
vscale c v = SR.times c <$> v

vmul :: SR.Semiring s => V ix s -> V ix s -> s
vmul xs ys = foldr SR.plus SR.zero (zipWith (SR.*) (M.elems xs) (M.elems ys))

norm2 :: (SR.Semiring s, Floating s) => V ix s -> s
norm2 v = sqrt (vmul v v)

-- End ---------------------------------------------------------------
