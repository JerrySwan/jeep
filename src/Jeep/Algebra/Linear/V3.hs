module Jeep.Algebra.Linear.V3 where

-----------------------------------  

import qualified Data.Semiring as SR

-----------------------------------  

data V3 a = V3 { _x :: a, _y :: a, _z :: a } deriving (Show,Eq,Ord)

-----------------------------------  

fromTuple :: (a,a,a) -> V3 a
fromTuple (x,y,z) = V3 x y z

toTuple :: V3 a -> (a,a,a)
toTuple v = (_x v,_y v, _z v)

-----------------------------------  

plus a b = V3 x' y' z' where 
  x' = SR.plus (_x a) (_x b)
  y' = SR.plus (_y a) (_y b)
  z' = SR.plus (_z a) (_z b)

minus a b = V3 x' y' z' where 
  x' = SR.minus (_x a) (_x b)
  y' = SR.minus (_y a) (_y b)
  z' = SR.minus (_z a) (_z b)

zero :: SR.Semiring a => V3 a  
zero = V3 SR.zero SR.zero SR.zero

one :: SR.Semiring a => V3 a  
one = V3 SR.one SR.one SR.one

negate v = V3 (SR.negate (_x v)) (SR.negate (_y v)) (SR.negate (_z v))

sqrNorm2 :: SR.Ring a => V3 a -> a  
sqrNorm2 v = SR.sum' [sqr (_x v),sqr (_y v),sqr (_z v)] where
  sqr x = SR.times x x

sqrEuclidianDistance :: SR.Ring a => V3 a -> V3 a -> a  
sqrEuclidianDistance a b = sqrNorm2 (minus a b)

-- End ---------------------------------------------------------------
