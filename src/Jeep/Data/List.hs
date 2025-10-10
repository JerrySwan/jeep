{-# LANGUAGE BangPatterns #-}
module Jeep.Data.List where

-----------------------------------

count :: (a -> Bool) -> [a] -> Int
count p = go 0 where 
  go !n [] = n
  go !n (x:xs) 
    | p x = go (n+1) xs
    | otherwise = go n xs

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

rotate :: Int -> [a] -> [a]
rotate n xs = take lxs . drop (n `mod` lxs) . cycle $ xs where
  lxs = length xs

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = let (ys, zs) = splitAt n xs in ys : chunks n zs

minmax :: (Ord a) => [a] -> Maybe (a,a)
minmax [] = Nothing
minmax (x:xs) = Just $ foldl (\(mn,mx) a -> (min mn a,max mx a)) (x,x) xs

 -- End --------------------------------------------------------------
