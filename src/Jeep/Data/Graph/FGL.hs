module Jeep.Data.Graph.FGL where

-----------------------------------
import Data.Graph.Inductive.Graph

-----------------------------------

implicitGraphToGraph :: Graph gr => Int -> (Int -> a) -> (Int -> Int -> b) -> gr a b
implicitGraphToGraph nNodes nLabel eLabel = mkGraph mkLNodes mkLEdges where
  mkLNodes = (\i -> (i,nLabel i)) <$> [0..nNodes-1] 
  mkLEdges = concat $ [ [ (i,j,eLabel i j) | i <- [0 .. nNodes-1] ] | j <- [0 .. nNodes-1] ] 

matrixToGraph :: Graph gr => [[a]] -> gr Int a
matrixToGraph xss = implicitGraphToGraph (length xss) id (\i j -> (xss !! i) !! j )

-- End ---------------------------------------------------------------
