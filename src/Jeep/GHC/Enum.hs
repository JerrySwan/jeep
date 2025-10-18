{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
module Jeep.GHC.Enum where

-----------------------------------

type BEnum a = (Enum a, Bounded a) 

-----------------------------------

enumAll :: BEnum a => [a]
enumAll = [minBound .. maxBound]

enumAll2 :: (BEnum a, BEnum b) => [(a,b)]
enumAll2 = [(x,y) | x <- enumAll, y <- enumAll]

tabulate :: (BEnum a) => (a -> b) -> [(a,b)]
tabulate f = [(x, f x) | x <- enumAll]

tabulate2 :: (BEnum a, BEnum b) => (a -> b -> c) -> [((a,b), c)]
tabulate2 f = [((x,y), f x y) | x <- enumAll, y <- enumAll]

-- End ---------------------------------------------------------------
