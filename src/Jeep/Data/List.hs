module Jeep.Data.List where

-----------------------------------

rotate :: Int -> [a] -> [a]
rotate n xs = take lxs . drop (n `mod` lxs) . cycle $ xs where
  lxs = length xs

 -- End --------------------------------------------------------------
