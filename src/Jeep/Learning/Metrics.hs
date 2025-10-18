module Jeep.Learning.Metrics where

-----------------------------------

import Control.Exception

import Jeep.Data.List

-----------------------------------

zeroOneLoss :: Eq a => [(a,a)] -> Int
zeroOneLoss xys = count (uncurry (/=)) xys where
  xs = fst <$> xys 
  ys = snd <$> xys 
  

{-

mse :: (Num a,Fractional a) => [a] -> [a] -> a
mse xs ys =
  assert (length xs == length ys)
  sum [ sqr (x-y) | (x,y) <- zip xs ys] / n where
    n = fromIntegral $ length xs
    sqr x = x * x

rmse :: Floating a => [a] -> [a] -> a
rmse xs ys = sqrt (mse xs ys)

-}

mse :: (Num a,Fractional a) => [(a,a)] -> a
mse xys = sum [ sqr (x-y) | (x,y) <- zip xs ys] / n where
  xs = fst <$> xys 
  ys = snd <$> xys 
  n = fromIntegral $ length xs
  sqr x = x * x

rmse :: Floating a => [(a,a)] -> a
rmse xys = sqrt (mse xys)

-- End ---------------------------------------------------------------
