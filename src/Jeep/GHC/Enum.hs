{-# LANGUAGE ConstraintKinds #-}
module Jeep.GHC.Enum where

-----------------------------------

type BEnum a = (Enum a, Bounded a) 

enumAll :: (BEnum a) => [a]
enumAll = [minBound .. maxBound]

-- End ---------------------------------------------------------------
