
{-# LANGUAGE LambdaCase #-}
module Jeep.Algebra.Polynomial where

-----------------------------------

import Data.Maybe

import Math.Polynomial

-----------------------------------

-- | Horrible hack, made necessary 
-- by Math.Polynomial.Poly's overly enthusiastic information-hiding  
endianness :: Show a => Poly a -> Endianness 
endianness p = fromMaybe undefined $ case take (length "poly ?E") (show p) of
  "poly LE" -> Just LE
  "poly BE" -> Just BE
  _ -> Nothing

-----------------------------------

-- | Adapted from https://rosettacode.org/wiki/Cyclotomic_polynomial#Haskell
showPolyBE :: Show a => [a] -> String
showPolyBE [] = "0"
showPolyBE p = foldl1 (\r -> (r ++) . term) $
  dropWhile null $
    foldMap (\(c, n) -> [show c ++ expt n]) $
      zip (reverse p) [0..] where
    expt = \case 
      0 -> ""
      1 -> "*x"
      n -> "*x^" ++ show n
    term = \case 
      [] -> ""
      '0':'*':t -> ""
      '-':'1':'*':t -> " - " ++ t
      '1':'*':t -> " + " ++ t
      '-':t -> " - " ++ t
      t -> " + " ++ t     

showPolyLE :: Show a => [a] -> String
showPolyLE = showPolyBE . reverse

-- End ---------------------------------------------------------------
