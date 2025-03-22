module Jeep.Data.Bag where

-----------------------------------

import Data.List
import Data.Maybe

import qualified Data.Set as Set
import qualified Data.Map as Map

-----------------------------------

newtype Bag a = Bag { asMap :: Map.Map a Int }

instance Show a => Show (Bag a) where
  show (Bag m) = "{" ++ (intercalate "," str) ++ "}" where
    str = f <$> Map.toList m 
    f (k,c) = show k ++ "*" ++ show c

-- https://www.reddit.com/r/haskell/comments/3an5xp/groupby_data_to_a_map/
bagify :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
bagify key as = Map.fromListWith (++) as' where
  as' = map ((,) <$> key <*> (:[])) as

fromList :: Ord a => [a] -> Bag a 
fromList xs = Bag $ (\ys -> length ys) <$> bagify id xs

isBag :: Integral count => Map.Map a count -> Bool
isBag m = all ((>=) 0) (Map.elems m)

fromMap :: Map.Map a Int -> Maybe (Bag a)
fromMap m = if isBag m then (Just $ Bag m ) else Nothing

count :: Ord a => Bag a -> a -> Int
count m x = fromMaybe 0 (Map.lookup x (asMap m)) 

keySet :: Bag a -> Set.Set a
keySet x = Map.keysSet (asMap x)

size :: Bag a -> Int
size m = sum $ Map.elems (asMap m)

isEmpty :: Bag a -> Bool
isEmpty m = size m == 0

-- End ---------------------------------------------------------------

