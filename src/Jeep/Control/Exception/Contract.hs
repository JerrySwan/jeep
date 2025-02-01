module Jeep.Control.Exception.Contract where

-----------------------------------

import Debug.Trace

-----------------------------------

require :: String -> Bool -> a -> a
require _ True = id
require msg False = \_ -> traceStack "Precondition Failed" $ error msg

invariant :: String -> Bool -> a -> a
invariant _ True = id
invariant msg False = \_ -> traceStack "Invariant Failed" $ error msg

ensure :: String -> Bool -> a -> a
ensure _ True = id
ensure msg False = \_ -> traceStack "Postcondition Failed" $ error msg

-- End ---------------------------------------------------------------
