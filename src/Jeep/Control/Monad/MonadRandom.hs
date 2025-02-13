module Jeep.Control.Monad.MonadRandom where

-----------------------------------

import Control.Monad.Random

-----------------------------------

uniformRandomIndex :: (MonadRandom random) => [a] -> random Int
uniformRandomIndex xs = getRandomR (0,length xs - 1)

uniformRandomElem :: (MonadRandom random) => [a] -> random a
uniformRandomElem xs = (xs !!) <$> uniformRandomIndex xs

-----------------------------------

{----------------
  Adapted from:
  https://stackoverflow.com/questions/13936502/sampling-a-normal-distribution-using-control-monad-monadrandom
----------------}

-- |Generates uniform random variables.
unif :: (MonadRandom m) => m Double
unif = getRandomR (0,1)

-- |Generate two samples from the standard normal distribution, using
--  the Box-Muller method.
stdNormals :: (MonadRandom m) => m (Double,Double)
stdNormals = do
    u <- unif
    v <- unif
    let r = sqrt((-2) * log u)
    let arg1 = cos (2 * pi * v)
    let arg2 = sin (2 * pi * v)
    return (r * arg1, r * arg2)

-- |Generate a single sample from the standard normal distribution, by
--  generating two samples and throwing away the second one.
stdNormal :: (MonadRandom m) => m Double
stdNormal = do
    (x,_) <- stdNormals
    return x

-- |Generate a sample from the standard normal distribution with a given
--  mean and variance.
normal :: (MonadRandom m) => Double -> Double -> m Double
normal mu sigma = do
    x <- stdNormal
    return $ mu + sigma * x 

-- End ---------------------------------------------------------------
