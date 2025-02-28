module Jeep.Algebra.Linear.Sparse.M where

-----------------------------------

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

mtabulate :: Ord ix => [ix] -> (ix -> ix -> a) -> M ix a
mtabulate ixs f = Mp.fromList [ (j,r j) | j <- ixs] where
  r j = Mp.fromList [ (i,f i j) | i <- ixs]

midentity :: (Ord ix, Eq s, SR.Semiring s) => [ix] -> M ix s
midentity ixs = mtabulate ixs kr where 
  kr i j = if i == j then SR.one else SR.zero

mconst :: (Ord ix, Eq s, SR.Semiring s) => [ix] -> s -> M ix s
mconst ixs k = mtabulate ixs (\_ _ -> k)

mzeroes :: (Ord ix, Eq s, SR.Semiring s) => [ix] -> M ix s
mzeroes ixs = mconst ixs SR.zero

mones :: (Ord ix, Eq s, SR.Semiring s) => [ix] -> M ix s
mones ixs = mconst ixs SR.one

-----------------------------------

{-
isSquare :: [[a]] -> Bool
isSquare [] = True
isSquare xss = all (\xs -> length xs == length (head xss)) xss

unitSquare = fromLists [
  ( "x", [ ("x",[0,0]), ("y",[1,0]) ])
  ,("y",  [ ("x",[0,1]), ("y",[1,1]) ])
  ]
-}

msparse :: Ord ix => [ix] -> [[a]] -> M ix a
msparse indices xss =
  assert (MD.msquare xss)  
  assert (length indices == length xss) 
  Mp.fromList (zip indices m') where
    m' = Mp.fromList . zip indices <$> xss

mdense :: M ix a -> [[a]]
mdense m = vdense <$> vdense m

mvcat :: [[a]] -> [[a]] -> [[a]]
mvcat xss yss = MD.mtranspose (mhcat (MD.mtranspose xss) (MD.mtranspose yss))

mhcat :: [[a]] -> [[a]] -> [[a]]
mhcat xss yss = xss ++ yss

mmeet :: (Ord ix, SR.Semiring a) => M ix a -> M ix a -> M ix a
mmeet x y = assert (indicesDisjoint x y) $ msparse indices matrix where
  indices = orderedIndices x ++ orderedIndices y
  cardx = length (orderedIndices x)
  cardy = length (orderedIndices y)
  tl = mdense x
  tr = MD.mzeroes cardx cardy
  bl = MD.mzeroes cardx cardy
  br = mdense y
  top = mhcat tl tr
  bottom = mhcat bl br
  matrix = mvcat top bottom
-- ^ https://hackage.haskell.org/package/containers-0.7/docs/Data-Map-Lazy.html#v:union
-- The expression (union t1 t2) takes the left-biased union of t1 and t2. 

fromLists :: Ord ix => [(ix,[(ix,s)])] -> M ix s
fromLists xss = Mp.fromList $ (\(k,v) -> (k,Mp.fromList v)) <$> xss

orderedIndices :: M ix a -> [ix]
orderedIndices m = fst <$> Mp.toList m

indicesDisjoint :: Ord ix => M ix a -> M ix b -> Bool
indicesDisjoint x y = S.disjoint (S.fromList (orderedIndices x)) (S.fromList (orderedIndices y)) 

{-

mscale :: SR.Semiring s => s -> M ix s -> M ix s
mscale c = map (map (SR.times c))

-}

mmul :: SR.Semiring s => M ix s -> M ix s -> M ix s
mmul = undefined
-- TODO


{-
mmul xss yss = go xss where
  cols = mtranspose yss
  go [] = []
  go (r:rs) = map (vmul r) cols:go rs
-}

{-
mvmul :: SR.Semiring s => M ix s -> V ix s -> V ix s
mvmul xss ys = head $ mtranspose (mmul xss (mtranspose [ys]))

vmmul :: SR.Semiring s => V ix s -> M ix s -> V ix s
vmmul ys xss = head $ mmul [ys] xss
-}

{-
sparseMatrixToDot :: Show a => SparseMatrix a -> String
sparseMatrixToDot m = intercalate "\n" ["digraph {", edges, "}"] where
    edges = intercalate "\n" $ concat ((\(i,mj) -> edge i <$> IM.toList mj ) <$> IM.toList m)
    edge i (j,v) = "v" ++ show i ++ " -> v" ++ show j ++ "[label=\"" ++ show v ++ "\"];" 
-}

-- End ---------------------------------------------------------------
