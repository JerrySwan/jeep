module Jeep.Logic.AU where

-----------------------------------

import Control.Exception
import qualified Data.Map as M

import Jeep.Logic.Term

-----------------------------------

type Subs = M.Map Var (Term,Term)

data AU = AU {
  term :: Term,
  subs :: Subs
} deriving (Show,Eq)

-----------------------------------

empty :: AU
empty = AU identityFn M.empty

au :: Term -> Subs -> AU
au = AU

update :: AU -> Term -> Subs -> AU
update a t s = au t (M.union (subs a) s)

compose :: AU -> AU -> AU
compose x y = update x (term y) (subs y)

freshVar :: AU -> Var
freshVar a = "u" ++ show (M.size (subs a))

freshSubs :: AU -> Term -> Term -> AU
freshSubs a t1 t2 = update a (V v) (M.singleton v (t1,t2)) where
  v = freshVar a

isAU :: AU -> Term -> Term -> Bool
isAU a x y = substitute (fst <$> subs a) (term a) == x && 
  substitute (snd <$> subs a) (term a) == y

----------------------------------

antiunify :: Term -> Term -> AU
antiunify a b = (assert $ isAU result a b) result where
  result = go a b empty 
  go :: Term -> Term -> AU -> AU
  go x y acc =
    if x == y then
      au x M.empty
    else
      case (x,y,acc) of
      (t1@(V _),t2@(V _),acc) -> freshSubs acc t1 t2
      (t1@(V _),t2@(F _ _),acc) -> freshSubs acc t1 t2
      (t1@(F _ _),t2@(V _),acc) -> freshSubs acc t1 t2
      (t1@(F id1 as1 ),t2@(F id2 as2 ),acc) ->
        if id1 == id2 && length as1 == length as2 then
          let
            f acc' (a1,a2) = compose acc' (go a1 a2 acc')
            accs = scanl f empty (zip as1 as2)
          in update acc (F id1 (term <$> tail accs)) (subs (last accs))
        else
          freshSubs acc t1 t2

-----------------------------------

(==>) :: Var -> (Term,Term) -> Subs
(==>) = M.singleton

test1 :: IO ()
test1 = do
  let expected = au (var "v0") M.empty
  let actual = antiunify (var "v0") (var "v0")
  assert ( actual == expected ) print "test passed."

test2 :: IO ()
test2 = do
  let expected = au (var "u0") ("u0" ==> (var "v0", var "v1"))
  let actual = antiunify (var "v0") (var "v1")
  assert ( actual == expected ) print "test passed."

test3 :: IO ()
test3 = do
  let t1 = var "v0"
  let t2 = F "f1" []
  let expected = au (var "u0") ("u0" ==> (t1, t2))
  let actual = antiunify t1 t2
  assert ( actual == expected ) print "test passed."

test4 :: IO ()
test4 = do
  let t1 = F "f1" []
  let t2 = var "v0"
  let expected = au (var "u0") ("u0" ==> (t1, t2))
  let actual = antiunify t1 t2
  assert ( actual == expected ) print "test passed."

test5 :: IO ()
test5 = do
  let t1 = F "f1" []
  let t2 = F "f1" []
  let expected = au t1 M.empty
  let actual = antiunify t1 t2
  assert ( actual == expected ) print "test passed."

test6 :: IO ()
test6 = do
  let t1 = F "f1" [var "v0"]
  let t2 = F "f1" []
  let expected = au (var "u0") ("u0" ==> (t1, t2))
  let actual = antiunify t1 t2
  assert ( actual == expected ) print "test passed."

test7 :: IO ()
test7 = do
  let t1 = F "f1" [var "v0"]
  let t2 = F "f1" [var "v1"]
  let expected = au (F "f1" [var "u0"]) ("u0" ==> (var "v0", var "v1"))
  let actual = antiunify t1 t2
  assert ( actual == expected ) print "test passed."

test8 :: IO ()
test8 = do
  let st1 = F "f2" [var "v1", var "v3"]
  let st2 = F "f2" [var "v2", var "v4"]
  let t1 = F "f1" [var "v0", st1]
  let t2 = F "f1" [var "v0", st2]
  let expectedTerm = F "f1" [var "v0",F "f2" [var "u0", var "u1"]]
  let expectedSubs = M.fromList [("u0",(var "v1",var "v2")),("u1",(var "v3",var "v4"))]
  let expected = au expectedTerm expectedSubs
  let actual = antiunify t1 t2
  assert ( actual == expected ) print "test passed."

test9 :: IO ()
test9 = do
  let st1 = F "f2" [var "v1", var "v2"]
  let st2 = F "f2" [var "v2", var "v3"]
  let t1 = F "f1" [var "v0", st1]
  let t2 = F "f1" [var "v0", st2]
  let expectedTerm = F "f1" [var "v0",F "f2" [var "u0", var "u1"]]
  let expectedSubs = M.fromList [("u0",(var "v1",var "v2")),("u1",(var "v2",var "v3"))]
  let expected = au expectedTerm expectedSubs
  let actual = antiunify t1 t2
  assert ( actual == expected ) print "test passed."

-----------------------------------

auMain :: IO ()
auMain = do
  sequence [test1, test2, test3, test4, test5, test6, test7, test8, test9]
  print ""

-- End ---------------------------------------------------------------
