module Jeep.Algebra.Linear.V where

-----------------------------------

import qualified Data.Semiring as SR

-----------------------------------

type V s = [s]

-----------------

vmul :: SR.Semiring s => V s -> V s -> s
vmul xs = foldr SR.plus SR.zero . zipWith (SR.*) xs

-- End ---------------------------------------------------------------
