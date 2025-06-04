module Jeep.Data.Bag where

-----------------------------------

import Data.List
import Data.Maybe

import qualified Data.Set as S
import qualified Data.Map as M

-----------------------------------

newtype Bag a = Bag { asMap :: M.Map a Int } deriving (Eq)

instance Show a => Show (Bag a) where
  show (Bag m) = "{" ++ intercalate "," str ++ "}" where
    str = f <$> M.toList m
    f (k,c) = show k ++ "*" ++ show c

-----------------------------------

fromList :: (Eq a,Ord a) => [a] -> Bag a
fromList xs = Bag $ M.fromList $ (\ys -> (head ys, length ys)) <$> group (sort xs)

fromListBy :: Ord a => (a -> a -> Bool) -> [a] -> Bag a
fromListBy f xs = Bag $ M.fromList $ (\ys -> (head ys, length ys)) <$> sort (groupBy f xs)


-- https://www.reddit.com/r/haskell/comments/3an5xp/groupby_data_to_a_map/
bagify :: Ord k => (v -> k) -> [v] -> M.Map k [v]
bagify key as = M.fromListWith (++) as' where
  as' = map ((,) <$> key <*> (:[])) as

-- fromList :: Ord a => [a] -> Bag a
-- fromList xs = Bag $ (\ys -> length ys) <$> bagify id xs

mapIsBag :: Integral count => M.Map a count -> Bool
mapIsBag m = all (0 >=) (M.elems m)

fromMap :: M.Map a Int -> Bag a
fromMap m = Bag $ M.filter (0 >=) m 

count :: Ord a => Bag a -> a -> Int
count m x = fromMaybe 0 (M.lookup x (asMap m))

keySet :: Bag a -> S.Set a
keySet x = M.keysSet (asMap x)

size :: Bag a -> Int
size m = sum $ M.elems (asMap m)

isEmpty :: Bag a -> Bool
isEmpty m = size m == 0

-- End ---------------------------------------------------------------

