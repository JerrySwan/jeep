module Jeep.Algebra.Linear.Dense.V2 where

-----------------------------------  

import qualified Data.Semiring as SR

-----------------------------------  

data V2 a = V2 { _x :: a, _y :: a }  deriving (Show,Eq,Ord)

instance Functor V2 where
  fmap f (V2 x y) = V2 (f x) (f y)

-----------------------------------  

fromTuple :: (a,a) -> V2 a
fromTuple (x,y) = V2 x y

toTuple :: V2 a -> (a,a)
toTuple v = (_x v,_y v)

-----------------------------------  

vplus :: SR.Semiring a => V2 a -> V2 a -> V2 a
vplus a b = V2 x' y' where 
  x' = SR.plus (_x a) (_x b)
  y' = SR.plus (_y a) (_y b)

vminus :: SR.Ring a => V2 a -> V2 a -> V2 a
vminus a b = V2 x' y' where 
  x' = SR.minus (_x a) (_x b)
  y' = SR.minus (_y a) (_y b)

vzero :: SR.Semiring a => V2 a  
vzero = V2 SR.zero SR.zero

vone :: SR.Semiring a => V2 a  
vone = V2 SR.one SR.one

vnegate :: SR.Ring a => V2 a -> V2 a
vnegate v = V2 (SR.negate (_x v)) (SR.negate (_y v))

-----------------------------------  

vsqrnorm2 :: SR.Ring a => V2 a -> a  
vsqrnorm2 v = SR.plus (sqr (_x v)) (sqr (_y v)) where
  sqr x = SR.times x x

vnorm2 :: (Floating a, SR.Ring a) => V2 a -> a
vnorm2 = sqrt. vsqrnorm2

vquadrance2 :: SR.Ring a => V2 a -> V2 a -> a  
vquadrance2 a b = vsqrnorm2 (vminus a b)

vdistance2 :: (Floating a, SR.Ring a) => V2 a -> V2 a -> a
vdistance2 x y = sqrt $ vquadrance2 x y

-- End ---------------------------------------------------------------
