module Jeep.Algebra.Linear.V where

-----------------------------------

import qualified Data.Semiring as SR

-----------------------------------

type V s = [s]

-----------------

vscale :: SR.Semiring s => s -> V s -> V s
vscale c = map (SR.times c)

vmul :: SR.Semiring s => V s -> V s -> s
vmul xs = foldr SR.plus SR.zero . zipWith (SR.*) xs

norm2 :: (SR.Semiring s, Floating s) => V s -> s
norm2 v = sqrt (vmul v v) 

-- End ---------------------------------------------------------------
