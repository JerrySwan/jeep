module Jeep.Algebra.Linear.Dense.V where

-----------------------------------

import qualified Data.Semiring as SR

-----------------------------------

type V s = [s]

-----------------

vscale :: (SR.Semiring s) => s -> V s -> V s
vscale c = map (SR.times c)

vadd :: (SR.Semiring s) => V s -> V s -> V s
vadd xs = zipWith (SR.+) xs

vsub :: (SR.Ring s) => V s -> V s -> V s
vsub xs = zipWith (SR.-) xs

vmul :: (SR.Semiring s) => V s -> V s -> s
vmul xs = foldr SR.plus SR.zero . zipWith (SR.*) xs

vnorm2 :: (SR.Semiring s, Floating s) => V s -> s
vnorm2 v = sqrt (vmul v v)

-- End ---------------------------------------------------------------
