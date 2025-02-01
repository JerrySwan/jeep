module Jeep.Logic.Term where

-----------------------------------

import Data.List
import Data.Maybe

import qualified Data.Map as M

-----------------------------------

type Var = String

data Term = V Var | F { uid :: String, args :: [Term] } deriving (Eq)

-----------------

var :: String -> Term
var = V

identityFn :: Term
identityFn = F "Id" []

-----------------

instance Show Term where
  show (V v) = show v
  show (F nm as) =
    if null as then
      show nm
    else
      show nm ++ "(" ++ intercalate "," (show <$> as) ++ ")"

-----------------------------------

vars :: Term -> [Var]
vars = go [] where
  go acc (V x) = acc ++ [x]
  go acc (F _ as) = acc ++ concatMap (go acc) as

substitute :: M.Map Var Term -> Term -> Term
substitute subs v@(V i) = fromMaybe v (M.lookup i subs)
substitute subs (F nm as) = F nm (substitute subs <$> as)

-- End ---------------------------------------------------------------
