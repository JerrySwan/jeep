module Jeep.Statistics.AIF where

-----------------------------------

import Data.List
import Data.Maybe  
import qualified Data.Map as Map

-----------------------------------

{-
module AIF_PAIF where

  #!/usr/bin/python
  # -*- coding: utf-8 -*-
  # code for the PAIF manuscript:
  # F. von Wegner, Partial Autoinformation to Characterize Symbolic Sequences
  # Front Physiol (2018) https://doi.org/10.3389/fphys.2018.01382
  # FvW 05/2018, Python3 version 05/2021
  
  import os, sys
  import numpy as np
  import matplotlib.pyplot as plt
  
  # --- define logarithm
  #log = np.log
  log = np.log2
  
  
  """*****************************************************************************
  !!! ALL SYMBOLS HAVE TO BE INTEGERS: 0, 1, 2, ... !!!
  *****************************************************************************"""
    
  def H_1(x, ns):
      """
      Shannon entropy of the symbolic sequence x with ns symbols.
      Args:
          x: symbolic sequence, symbols = [0, 1, ..., ns-1]
          ns: number of symbols
      Returns:
          h: Shannon entropy of x
      """
  
      n = len(x)
      p = np.zeros(ns)  # symbol distribution
      for t in range(n):
          p[x[t]] += 1.0
      p /= n
      h = -np.sum(p[p>0]*log(p[p>0]))
      return h
  
  
  def H_2(x, y, ns):
      """
      Joint Shannon entropy of the symbolic sequences x, y, with ns symbols.
      Args:
          x, y: symbolic sequences, symbols = [0, 1, ..., ns-1]
          ns: number of symbols
      Returns:
          h: joint Shannon entropy of (x, y)
      """
  
      if (len(x) != len(y)):
          print("H_2 warning : sequences of different lengths, using the shorter...")
      n = min(len(x), len(y))
      p = np.zeros((ns, ns)) # joint distribution
      for t in range(n):
          p[x[t],y[t]] += 1.0
      p /= n
      h = -np.sum(p[p>0]*log(p[p>0]))
      return h
  
  
  def H_k(x, ns, k):
      """
      Joint Shannon entropy of k-histories x[t:t+k]
      Args:
          x: symbolic sequence, symbols = [0, 1, ..., ns-1]
          ns: number of symbols
          k: length of k-history
      Returns:
          h: joint Shannon entropy of x[t:t+k]
      """
  
      n = len(x)
      p = np.zeros(tuple(k*[ns]))  # symbol joint distribution
      for t in range(n-k):
          p[tuple(x[t:t+k])] += 1.0
      p /= (n-k) # normalize distribution
      h = -np.sum(p[p>0]*log(p[p>0]))
      #m = np.sum(p>0)
      #h = h + (m-1)/(2*N) # Miller-Madow bias correction
      return h
  
  
  def ais(x, ns, k=1):
      """
      Active information storage (AIS)
      TeX notation:
      I(X_{n+1} ; X_{n}^{(k)})
      = H(X_{n+1}) - H(X_{n+1} | X_{n}^{(k)})
      = H(X_{n+1}) - H(X_{n+1},X_{n}^{(k)}) + H(X_{n}^{(k)})
      = H(X_{n+1}) + H(X_{n}^{(k)}) - H(X_{n+1}^{(k+1)})
      Args:
          x: symbolic sequence, symbols = [0, 1, ..., ns-1]
          ns: number of symbols
          k: history length (optional, default value k=1)
      Returns:
          a: active information storage
      """
  
      n = len(x)
      h1 = H_k(x, ns, 1)
      h2 = H_k(x, ns, k)
      h3 = H_k(x, ns, k+1)
      a = h1 + h2 - h3
      return a
  
  
  def aif(x, ns, kmax):
      """
      Time-lagged mutual information = Auto-information function (AIF)
      TeX notation:
      I(X_{n+k} ; X_{n})
      = H(X_{n+k}) - H(X_{n+k} | X_{n})
      = H(X_{n+k}) - H(X_{n+k},X_{n}) + H(X_{n})
      = H(X_{n+k}) + H(X_{n}) - H(X_{n+k},X_{n})
      Args:
          x: symbolic sequence, symbols = [0, 1, 2, ...]
          ns: number of symbols
          kmax: maximum time lag
      Returns:
          a: time-lagged mutual information array for lags k=0, ..., kmax-1
      """
  
      n = len(x)
      a = np.zeros(kmax)
      for k in range(kmax):
          nmax = n-k
          h1 = H_1(x[:nmax], ns)
          h2 = H_1(x[k:k+nmax], ns)
          h12 = H_2(x[:nmax], x[k:k+nmax], ns)
          a[k] = h1 + h2 - h12
          #if (k%10 == 0): print(f"\tAIF(k={k:d})\r", end="")
          print(f"\tAIF(k={k:d})\r", end="")
      print("")
      a /= a[0]  # normalize: a[0]=1.0
      return a
  
  
  def paif(x, ns, kmax):
      """
      Partial auto-information function (PAIF).
      TeX notation:
      I(X_{n+k} ; X_{n} | X_{n+k-1}^{(k-1)})
      = H(X_{n+k} | X_{n+k-1}^{(k-1)}) - H(X_{n+k} | X_{n+k-1}^{(k-1)}, X_{n})
      =   H(X_{n+k},X_{n+k-1}^{(k-1)}) - H(X_{n+k-1}^{(k-1)})
        - H(X_{n+k},X_{n+k-1}^{(k)}) + H(X_{n+k-1}^{(k)})
      = H(X_{n+k}^{(k)}) - H(X_{n+k-1}^{(k-1)}) - H(X_{n+k}^{(k+1)}) + H(X_{n+k-1}^{(k)})
      Args:
          x: symbolic sequence, symbols = [0, 1, 2, ...]
          ns: number of symbols
          kmax: maximum time lag
      Returns:
          p: partial autoinformation array for lags k=0, ..., kmax-1
      """
  
      n = len(x)
      p = np.zeros(kmax)
      a = aif(x, ns, 2)  # use AIF coeffs. for k=0, 1
      p[0], p[1] = a[0], a[1]
      for k in range(2,kmax):
          h1 = H_k(x, ns, k)
          h2 = H_k(x, ns, k-1)
          h3 = H_k(x, ns, k+1)
          p[k] = 2*h1 - h2 - h3
          #if (k%5 == 0): print(f"\tPAIF(k={k:d})\r", end="")
          print(f"\tPAIF(k={k:d})\r", end="")
      print("")
      p /= p[0]  # normalize: p[0]=1.0
      return p
-}      

-----------------------------------

-- https://www.reddit.com/r/haskell/comments/3an5xp/groupby_data_to_a_map/
groupby :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupby key as = Map.fromListWith (++) as' where
  as' = map ((,) <$> key <*> (:[])) as

bagify :: Ord a => [a] -> Map.Map a Int
bagify xs = (\ys -> length ys) <$> groupby id xs

isBag :: Map.Map a Int -> Bool
isBag m = all ((>=) 0) (Map.elems m)

card :: Map.Map a Int -> Int
card m = sum (Map.elems m)

-- shannonEntropy :: [Integer] -> Integer -> Double
-- shannonEntropy xs alphabetSize = -h where
shannonEntropy xs = -h where  
    h = sum $ (\p -> p * logBase 2 p) <$> probs    
    probs = (\count -> fromIntegral count / fromIntegral n ) <$> bagify xs
    n = length xs

-- h = sum $ (\p -> p * logBase 2 p) <$> probs (xs ++ ys)
-- probs xs = (\count -> fromIntegral count / fromIntegral (card (bagify xs)) ) <$> bagify xs    

{-
jointShannonEntropy xs ys = -h where  
    n = min (length xs) (length ys) 
    h = sum $ (\p -> p * logBase 2 p) <$> jointProbs  
    jointProbs = [ v1*v2 | (k1,v1) <- probs xs, (k2,v2) <- probs ys ]
    probs as = Map.toList $ (\c -> fromIntegral c / fromIntegral (n+n) ) <$> bagify as    
-}

{-
jointShannonEntropy :: [Int] -> [Int] -> Double
jointShannonEntropy xs ys = -h where  
    n = min (length xs) (length ys) 
    h = sum $ (\p -> p * logBase 2 p) <$> jointProbs  
    symbols = nub (xs ++ ys)    
    jointProbs = [ (pr s1 xs)*(pr s2 ys) | s1 <- symbols, s2 <- symbols ]    
    pr a as = f (Map.lookup a bag) where 
      bag = bagify as      
      f Nothing = (fromIntegral 0)
      f ( Just count ) = fromIntegral count / fromIntegral n -- (card bag)
-}

-- jointShannonEntropy :: [Int] -> [Int] -> Double
jointShannonEntropy xs ys = -h where  
  h = sum $ (\p -> p * logBase 2 p) <$> Map.elems jointProbs      
  n = min (length xs) (length ys) 
  jointProbs = probs $ (\t -> (xs !! t,ys !! t)) <$> [0 .. (n-1)]
  probs as = (\c -> fromIntegral c / fromIntegral n ) <$> bagify as      

-- a.k.a. 'time-lagged mutual information'?    
-- jointShannonEntropyOfHistories :: Ord a => [a] -> Int -> Double   
jointShannonEntropyOfHistories xs k = -h where
  n = length xs
  h = sum $ (\p -> p * logBase 2 p) <$> probs 
  probs = (\count -> fromIntegral count / fromIntegral (n-k) ) <$> (bagify $ windows k xs)
  windows n as = filter (\xs -> length xs >= n) $ Data.List.transpose (take n (tails as)) 
            
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

aif :: (Ord a, Floating b) => [a] -> Int -> [b]
aif x kmax = (\x -> x / head array) <$> array where
-- kmax: maximum time lag
-- returns: time-lagged mutual information array for lags k=0, ..., kmax-1
  n = length x
  array = f <$> [0 .. kmax-1]
  f k = h1 + h2 - h12 where 
    nmax = n - k    
    a1 = slice 0 (nmax-1) x
    a2 = slice k (k+nmax-1) x
    h1 = shannonEntropy a1 
    h2 = shannonEntropy a2 
    h12 = jointShannonEntropy a1 a2 -- alphabetSize

{-
def paif(x, ns, kmax):
    n = len(x)
    p = np.zeros(kmax)
    a = aif(x, ns, 2)  # use AIF coeffs. for k=0, 1
    p[0], p[1] = a[0], a[1]
    for k in range(2,kmax):
        h1 = H_k(x, ns, k)
        h2 = H_k(x, ns, k-1)
        h3 = H_k(x, ns, k+1)
        p[k] = 2*h1 - h2 - h3
    p /= p[0]  # normalize: p[0]=1.0
    return p
  -}

paif :: (Ord a, Floating b) => [a] -> Int -> [b]
paif xs kmax = (\x -> x / head p) <$> p where
-- returns: partial autoinformation array for lags k=0, ..., kmax-1
    n = length xs
    a = aif xs 2 -- # use AIF coeffs. for k=0, 1
    p = f <$> [0 .. kmax] where
        f 0 = a !! 0        
        f 1 = a !! 1
        f k = (2*h1) - h2 - h3 where
            h1 = jointShannonEntropyOfHistories xs k
            h2 = jointShannonEntropyOfHistories xs (k-1)
            h3 = jointShannonEntropyOfHistories xs (k+1)

order :: (Ord a1, Ord a2, Floating a1) => [a2] -> Int -> a1 -> [a1]
order xs kmax epsilon = takeWhile (\x -> abs x > epsilon ) (paif xs kmax)

-- End ---------------------------------------------------------------

