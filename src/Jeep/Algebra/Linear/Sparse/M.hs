module Jeep.Algebra.Linear.Sparse.M where

-----------------------------------

import Data.List
import Control.Exception

import qualified Data.Map as Mp
import qualified Data.Set as S

import qualified Data.Semiring as SR

-- import Jeep.Data.List
-- import qualified Jeep.Algebra.Linear.Dense.V as VD
import qualified Jeep.Algebra.Linear.Dense.M as MD

import Jeep.Algebra.Linear.Sparse.V

-----------------------------------

type M ix s = V ix (V ix s)

-----------------------------------

mnrows :: M ix a -> Int
mnrows = length

mvalidcols :: M ix a -> Bool
mvalidcols m = MD.mvalidcols (mdense m)

mncols :: M ix a -> Int
mncols m = MD.mncols (mdense m)

isSquare :: M ix a -> Bool
isSquare m = MD.isSquare (mdense m) 

-----------------------------------

mrowindices :: M ix a -> [ix]
mrowindices m = fst <$> Mp.toList m

mcolindices :: M ix a -> [ix]
mcolindices m = undefined

mtabulate :: Ord ix => [ix] -> [ix] -> (ix -> ix -> a) -> M ix a
mtabulate rixs cixs f = Mp.fromList [ (j,r j) | j <- cixs] where
  r j = Mp.fromList [ (i,f i j) | i <- rixs]

midentity :: (Ord ix, Eq s, SR.Semiring s) => [ix] -> M ix s
midentity ixs = mtabulate ixs ixs kr where 
  kr i j = if i == j then SR.one else SR.zero

mconst :: (Ord ix, Eq s, SR.Semiring s) => [ix] -> [ix] -> s -> M ix s
mconst rixs cixs k = mtabulate rixs cixs (\_ _ -> k)

mzeros :: (Ord ix, Eq s, SR.Semiring s) => [ix] -> [ix] -> M ix s
mzeros rixs cixs = mconst rixs cixs SR.zero

mones :: (Ord ix, Eq s, SR.Semiring s) => [ix] -> [ix] -> M ix s
mones rixs cixs = mconst rixs cixs SR.one

-----------------------------------

fromLists :: Ord ix => [(ix,[(ix,s)])] -> M ix s
fromLists xss = Mp.fromList $ (\(k,v) -> (k,Mp.fromList v)) <$> xss

{-
project :: Ord ix => [ix] -> [ix] -> [[a]] -> M ix a
project rixs cixs xss = mtabulate rixs cixs f where
  f i j = (xss !! ixi) !! ixj where 
    ixi = findIndex rixs i
    ixj = findIndex cixs j
  -- assert (MD.isSquare xss)  
  -- assert (length indices == length xss) 
    -- m' = Mp.fromList . zip cixs <$> xss  
    -- Mp.fromList (zip rixs m')
-}    

fromDense :: [[a]] -> M Int a
fromDense m = mtabulate rixs cixs f where
  rixs = [0 .. MD.mnrows m-1]
  cixs = [0 .. MD.mnrows m-1]
  f i j = (m !! i) !! j

mdense :: M ix a -> [[a]]
mdense m = vdense <$> vdense m

-----------------------------------

rowIndicesDisjoint :: Ord ix => M ix a -> M ix b -> Bool
rowIndicesDisjoint x y = S.disjoint (S.fromList (mrowindices x)) (S.fromList (mrowindices y)) 

colIndicesDisjoint :: Ord ix => M ix a -> M ix b -> Bool
colIndicesDisjoint x y = S.disjoint (S.fromList (mcolindices x)) (S.fromList (mcolindices y)) 

indicesDisjoint :: Ord ix => M ix a -> M ix b -> Bool
indicesDisjoint x y = rowIndicesDisjoint x y && colIndicesDisjoint x y 

mmeet :: (Ord ix, SR.Semiring a) => M ix a -> M ix a -> M ix a
mmeet x y = assert (indicesDisjoint x y) $ undefined rixs' cixs' matrix where
  rixs' = mrowindices x ++ mrowindices y
  cixs' = mcolindices x ++ mcolindices y
  matrix = MD.mmeet (mdense x) (mdense y)

-- ^ https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Lazy.html#v:union
-- The expression (union t1 t2) takes the left-biased union of t1 and t2. 

mscale :: SR.Semiring s => s -> M ix s -> M ix s
mscale c m = (SR.times c <$>) <$> m

{-
mmul :: SR.Semiring s => M ix s -> M ix s -> M ix s
mmul xss yss = 
  go xss where
  cols = mtranspose yss
  go [] = []
  go (r:rs) = map (vmul r) cols : go rs
-}

-- TODO



{-
vmmul :: SR.Semiring s => V ix s -> M ix s -> V ix s
vmmul ys xss = head $ mmul [ys] xss

mmul xss yss = go xss where
  cols = mtranspose yss
  go [] = []
  go (r:rs) = map (vmul r) cols : go rs
-}

{-
mvmul :: SR.Semiring s => M ix s -> V ix s -> V ix s
mvmul xss ys = head $ mtranspose (mmul xss (mtranspose [ys]))


-}

mgraphvizdot :: (Show ix,Show a) => M ix a -> String
mgraphvizdot m = intercalate "\n" ["digraph {", edges, "}"] where
  edges = intercalate "\n" $ concat ((\(i,mj) -> edge i <$> Mp.toList mj ) <$> Mp.toList m)
  edge i (j,v) = "v" ++ show i ++ " -> v" ++ show j ++ "[label=\"" ++ show v ++ "\"];" 

-- End ---------------------------------------------------------------
