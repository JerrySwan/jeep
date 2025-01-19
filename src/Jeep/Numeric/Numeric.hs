module Jeep.Numeric.Numeric where

------------------------------

mean :: (Fractional a, Foldable t) => t a -> a
mean = (/) <$> sum <*> realToFrac . length

sd :: (Floating a,Fractional a, Foldable t, Functor t) => t a -> a
sd xs = sqrt . mean $ (\x -> sqr (x - mu)) <$> xs where  
  sqr x = x*x
  mu = mean xs

-- End ---------------------------------------------------------------
