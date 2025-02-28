module Main (main) where

-----------------------------------

import System.Exit
import Test.HUnit

import qualified TestLinearAlgebra

-----------------------------------

main :: IO ()
main = do
  results <- runTestTT TestLinearAlgebra.tests
  if errors results + failures results == 0 then
    exitSuccess
  else
    exitFailure 

-- End ---------------------------------------------------------------

