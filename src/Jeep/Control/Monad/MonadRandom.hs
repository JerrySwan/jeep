module Jeep.Control.Monad.MonadRandom where

-----------------------------------

import Control.Monad.Random

import Data.Tuple
import Data.Ratio

-----------------------------------

{-

Use the following from Control.Monad.Random.Class instead:

uniformRandomIndex: (xs !!) <$> uniform [0.. length xs-1]

uniformRandomElem: uniform

weightedSelect: fromList/weighted

-----------------------------------

uniformRandomIndex :: (MonadRandom random) => [a] -> random Int
uniformRandomIndex xs = getRandomR (0,length xs - 1)

uniformRandomElem :: (MonadRandom random) => [a] -> random a
uniformRandomElem xs = (xs !!) <$> uniformRandomIndex xs

weightedSelect :: MonadRandom m => [a] -> (a -> Double) -> m a
weightedSelect xs fitness = do
  rand <- (sumScores *) <$> getRandom
  return $ (fst . head . dropWhile ((rand >) . snd)) xs' where
  fs = fitness <$> xs
  xs' = zip xs (scanl1 (+) fs)
  sumScores = (snd . last) xs'

-}

-----------------------------------

oneof :: MonadRandom m => [m b] -> m b
oneof [] = error "MonadRandom.oneof used with empty list"
oneof gs = getRandomR (0,length gs - 1) >>= (gs !!)

vectorOf :: (MonadRandom m {-, Random a -}) => Int -> m a -> m [a]
vectorOf = replicateM

elements :: MonadRandom m => [a] -> m a 
elements [] = error "MonadRandom.elements used with empty list"
elements xs = (xs !!) `fmap` getRandomR (0, length xs - 1)

frequency :: (Functor t, Foldable t, MonadRandom m) => t (Rational, m a) -> m a 
frequency xs = do
  x <- (weighted . (fmap swap)) xs
  x

suchThat :: (MonadRandom m, Random a) => m a -> (a -> Bool) -> m a 
suchThat mx p = do 
  x <- mx
  if p x then pure x else suchThat mx p

-----------------------------------

randomRational :: MonadRandom m => m Rational
randomRational = do
  n <- getRandom
  d <- getRandom `suchThat` (/= 0)
  return $ n % d 

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
