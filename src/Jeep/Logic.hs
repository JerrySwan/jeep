module Jeep.Logic where

-----------------------------------

implies :: Bool -> Bool -> Bool
implies a b = not a || b

iff :: Bool -> Bool -> Bool
iff = (==)

-- End ---------------------------------------------------------------
