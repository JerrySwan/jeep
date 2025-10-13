module Jeep.Numeric.Numeric where

-----------------------------------

import Jeep.Logic
import Control.Exception

-----------------------------------

linspace :: Fractional b => Int -> b -> b -> [b]
linspace n a b = map (\ i -> a + fromIntegral i * inc) [(0::Int) .. (n - 1)] where
  inc = (b - a) / fromIntegral (n - 1)

-----------------------------------

{- linear interpolation -}
lerp inputValue inputLimit1 inputLimit2 outputLimit1 outputLimit2 = 
  (assert $ implies (inputLimit1 <= inputLimit2) (inputLimit1 <= inputValue && inputValue <= inputLimit2) )
  (assert $ implies (inputLimit1 >= inputLimit2) (inputLimit2 <= inputValue  && inputValue <= inputLimit1) )
  (assert $ implies (inputLimit1 == inputLimit2) (inputValue == inputLimit1) ) 
  (assert $ (outputLimit1 <= outputLimit2) `implies` (outputLimit1 <= result  && result <= outputLimit2) )
  (assert $ (outputLimit1 >= outputLimit2) `implies` (outputLimit2 <= result && result <= outputLimit1) )
  result where 
  t = ( inputValue - inputLimit1 ) / ( inputLimit2 - inputLimit1 ) 
  r1 = (assert $ 0 <= t) (assert $ t <= 1) outputLimit1 * ( 1 - t )
  result = r1 + (outputLimit2 * t)

-----------------------------------

-- https://stackoverflow.com/questions/35529211/chinese-remainder-theorem-haskell

chineseRemainderTheorem :: (Integral a, Foldable t) => t (a, a) -> (a, a)
chineseRemainderTheorem = foldr go (0, 1) where
  go (r1, m1) (r2, m2) = (r `mod` m, m) where
    r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
    m = m2 * m1
  -- Modular Inverse
  a `inv` m = let (_, i, _) = gcd' a m in i `mod` m
  -- Extended Euclidean Algorithm
  gcd' 0 b = (b, 0, 1)
  gcd' a b = (g, t - (b `div` a) * s, s) where 
    (g, s, t) = gcd' (b `mod` a) a

-- End ---------------------------------------------------------------
