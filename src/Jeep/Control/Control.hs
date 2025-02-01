module Jeep.Control.Control( 
    module Jeep.Control.Monad.MonadRandom
    , module Jeep.Control.Exception.Contract
    , fixedpoint  
    )
where

------------------------------------

import Jeep.Control.Monad.MonadRandom
import Jeep.Control.Exception.Contract

------------------------------------

fixedpoint :: Eq a => (a -> a) -> a -> a
fixedpoint f x
  | x' == x = x'
  | otherwise = fixedpoint f x'
  where x' = f x

-- End ---------------------------------------------------------------
