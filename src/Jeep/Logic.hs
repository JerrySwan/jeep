module Jeep.Logic where

-----------------------------------

implies :: Bool -> Bool -> Bool
implies a b = if a then b else True

iff :: Bool -> Bool -> Bool
iff = (==)

-- End ---------------------------------------------------------------
