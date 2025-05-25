module Jeep.Data.Graph.FGL where

-----------------------------------

import System.Process ( readProcessWithExitCode )
import System.Exit

import Data.Graph.Inductive.Graph

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

-- End ---------------------------------------------------------------
