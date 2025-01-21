module Jeep.Statistics.Statistics where

------------------------------

mean :: (Fractional a, Foldable t) => t a -> a
mean = (/) <$> sum <*> realToFrac . length

variance :: (Fractional a, Foldable t, Functor t) => t a -> a
variance xs = mean $ (\x -> sqr (x - mu)) <$> xs where  
  sqr x = x*x
  mu = mean xs

sd :: (Floating a,Fractional a, Foldable t, Functor t) => t a -> a
sd = sqrt . variance

------------------------------

-- End ---------------------------------------------------------------
