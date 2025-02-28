module TestLinearAlgebra where

-----------------------------------

import qualified Data.Map as Mp

import qualified Jeep.Algebra.Linear.Dense.M as MD
import qualified Jeep.Algebra.Linear.Sparse.M as MS
import qualified Jeep.Algebra.Linear.Dense.V as VD
import qualified Jeep.Algebra.Linear.Sparse.V as VS

import Test.HUnit

import Test.QuickCheck

-----------------------------------

genVD :: Arbitrary a => Int -> Gen (VD.V a)
genVD n = vectorOf n arbitrary

genMD :: Arbitrary a => (Int,Int) -> Gen (MD.M a)
genMD (r,c) = vectorOf r (vectorOf c arbitrary)

genVS :: Arbitrary a => Int -> Gen (VS.V Int a)
genVS sz = f <$> genVD sz where
  f xs = VS.vsparse [0..length xs-1] xs

genMS :: Arbitrary a => (Int,Int) -> Gen (MS.M Int a)
genMS p@(r,c) = f <$> genMD p where
  n = max r c 
  f xss = MS.msparse xss

-----------------------------------

test_mnrows_mncols_dense :: Test
test_mnrows_mncols_dense = TestCase ( do
    let m = [[1,2,3],[1,2,3]] 
    assertEqual "" (MD.mnrows m) 2
    assertEqual "" (MD.mncols m) 3
  )

test_mvcat_dense :: Test
test_mvcat_dense = TestCase ( do
    let actual = MD.mvcat [[1]] [[2]] 
    let expected = [[1],[2]]
    assertEqual "" expected actual 
  )

test_mhcat_dense :: Test
test_mhcat_dense = TestCase ( do
    let actual = MD.mhcat [[1]] [[2,3]] 
    let  expected = [[1,2,3]]
    assertEqual "" expected actual 
  )

test_mmeet_dense :: Test
test_mmeet_dense = TestCase ( do
    let 
      expected = [
        [1,0,0],
        [0,2,3]
          ] :: [[Int]]
    let actual = MD.mmeet [[1]] [[2,3]] 
    assertEqual ("<<" ++ show actual ++ ">>") expected actual 
  )

-----------------------------------

test_mnrows_mncols_sparse :: Test
test_mnrows_mncols_sparse = TestCase ( do
    let m = MS.msparse [[1,2,3],[1,2,3]] 
    assertEqual "" (MS.mnrows m) 2
    assertEqual "" (MS.mncols m) 3
  )

test_mmeet_sparse :: Test
test_mmeet_sparse = TestCase ( do
    let 
      expected = MS.msparse [
        [1::Integer,0,0],
        [0,2,3]
          ] 
    let actual = MS.mmeet (MS.msparse [[1]]) (MS.msparse [[2,3]]) 
    assertEqual "" expected actual 
  )

-----------------------------------

tests :: Test
tests = TestList [
    TestLabel "test_mnrows_mncols_dense" test_mnrows_mncols_dense
    , TestLabel "test_mvcat_dense" test_mvcat_dense    
    , TestLabel "test_mhcat_dense" test_mhcat_dense
    , TestLabel "test_mmeet_dense" test_mmeet_dense
    , TestLabel "test_mnrows_mncols_sparse" test_mnrows_mncols_sparse
    , TestLabel "test_mmeet_sparse" test_mmeet_sparse
  ]

-- End ---------------------------------------------------------------
