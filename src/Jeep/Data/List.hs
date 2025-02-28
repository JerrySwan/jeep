module Jeep.Data.List where

-----------------------------------

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs

rotate :: Int -> [a] -> [a]
rotate n xs = take lxs . drop (n `mod` lxs) . cycle $ xs where
  lxs = length xs

minmax :: (Ord a) => [a] -> Maybe (a,a)
minmax [] = Nothing
minmax (x:xs) = Just $ foldl (\(mn,mx) a -> (min mn a,max mx a)) (x,x) xs

 -- End --------------------------------------------------------------
