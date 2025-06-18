module Jeep.Test.QuickCheck where

-----------------------------------

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

-----------------------------------

seeded :: Int -> Gen a -> Gen a
seeded seed (MkGen g) = MkGen (\_ n -> g (mkQCGen seed) n ) 

-- End ---------------------------------------------------------------
