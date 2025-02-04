module Jeep.Algebra.Linear.V2 where

-----------------------------------  

import qualified Data.Semiring as SR

-----------------------------------  

data V2 a = V2 { _x :: a, _y :: a }  deriving (Show,Eq,Ord)

-----------------------------------  

fromTuple :: (a,a) -> V2 a
fromTuple (x,y) = V2 x y

toTuple :: V2 a -> (a,a)
toTuple v = (_x v,_y v)

-----------------------------------  

plus a b = V2 x' y' where 
  x' = SR.plus (_x a) (_x b)
  y' = SR.plus (_y a) (_y b)

minus a b = V2 x' y' where 
  x' = SR.minus (_x a) (_x b)
  y' = SR.minus (_y a) (_y b)

zero :: SR.Semiring a => V2 a  
zero = V2 SR.zero SR.zero

one :: SR.Semiring a => V2 a  
one = V2 SR.one SR.one

negate v = V2 (SR.negate (_x v)) (SR.negate (_y v))

-----------------------------------  

sqrNorm2 :: SR.Ring a => V2 a -> V2 a -> a  
sqrNorm2 v = SR.plus (sqr (_x v)) (sqr (_y v)) where
  sqr x = SR.times x x

sqrEuclidianDistance :: SR.Ring a => V2 a -> V2 a -> a  
sqrEuclidianDistance a b = sqrNorm2 (minus a b)

-- End ---------------------------------------------------------------
