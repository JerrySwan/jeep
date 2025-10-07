{-# LANGUAGE FlexibleContexts #-}
module Jeep.Data.Graph.FGL where

-----------------------------------

import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.RWS

import Data.DList (singleton, fromList, toList)
import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import qualified Data.Map as M
import Data.Maybe
import Data.Tree

import Data.Graph.Inductive.Graph

import System.Process ( readProcessWithExitCode )
import System.Exit

-----------------------------------

implicitGraphToGraph :: Graph gr => Int -> (Int -> a) -> (Int -> Int -> b) -> gr a b
implicitGraphToGraph nNodes nLabel eLabel = mkGraph mkLNodes mkLEdges where
  mkLNodes = (\i -> (i,nLabel i)) <$> [0..nNodes-1] 
  mkLEdges = concat $ [ [ (i,j,eLabel i j) | i <- [0 .. nNodes-1] ] | j <- [0 .. nNodes-1] ] 

matrixToGraph :: Graph gr => [[a]] -> gr Int a
matrixToGraph xss = implicitGraphToGraph (length xss) id (\i j -> (xss !! i) !! j )

-----------------------------------

callCmd :: String -> [String] -> IO (ExitCode, String, String)
callCmd cmd args = readProcessWithExitCode cmd args stdIn where
  stdIn = ""

invokeGraphvizDot :: FilePath -> String -> IO ExitCode
invokeGraphvizDot dotfilePath outputType = do
  (exitCode, stdOut, stdErr) <- callCmd "dot" [dotfilePath,"-T" ++ outputType,"-O" ]
  return exitCode

-----------------------------------

-- | https://stackoverflow.com/questions/13355968/elegant-way-to-convert-a-tree-to-a-functional-graph-library-tree

treeToGraph :: Tree a -> Gr a ()
treeToGraph t = uncurry mkGraph . (toList *** toList) . snd $ evalRWS (go t) () [0..] where
  go (Node a ns) = do
    i <- state $ head &&& tail
    es <- forM ns $ go >=> \j -> return (i, j, ())
    tell (singleton (i, a), fromList es)
    return i

relabelGraph :: Gr a b -> (Node -> Node) -> Gr a b
relabelGraph g f = mkGraph lnodes' ledges' where
  lnodes' = (\(n,a) -> (f n,a)) <$> labNodes g
  ledges' = (\(n1,n2,b) -> (f n1,f n2,b)) <$> labEdges g

maxNode :: Gr a b -> Maybe Node
maxNode g = if isEmpty g then Nothing else Just (snd (nodeRange g))

insGraph :: Gr a b -> Gr a b -> Gr a b
insGraph g1 g2 = -- trace (show (graphNodeRange g1) ++ " " ++ show (graphNodeRange g2) ) 
  mkGraph (labNodes g1 ++ labNodes g2') (labEdges g1 ++ labEdges g2') where
  nodeInc = fromMaybe 0 ((+ 1)  <$> maxNode g1)
  g2' = relabelGraph g2 (+ nodeInc)
  g1LNodes = labNodes g1

forestToGraph :: [Tree a] -> Gr a ()
forestToGraph ts = foldr insGraph empty gs where
  gs = treeToGraph <$> ts

-- End ---------------------------------------------------------------
