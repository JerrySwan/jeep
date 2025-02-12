module Jeep.Learning.Metrics where

-----------------------------------

import Control.Exception

-----------------------------------

mse :: (Num a,Fractional a) => [a] -> [a] -> a 
mse xs ys = 
  assert (length xs == length ys) 
  sum [ sqr (x-y) | (x,y) <- zip xs ys] / n where 
    n = fromIntegral $ length xs
    sqr x = x * x

rmse :: Floating a => [a] -> [a] -> a
rmse xs ys = sqrt (mse xs ys)

-- End ---------------------------------------------------------------
