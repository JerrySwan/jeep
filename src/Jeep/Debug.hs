{-# LANGUAGE RecordWildCards #-}

module Jeep.Debug where

-----------------------------------

import Debug.Trace
import GHC.Stack

-----------------------------------

here' :: HasCallStack => Int -> String 
here' callStackIndex = prettySrcLoc loc where
  loc = (snd $ (getCallStack callStack) !! callStackIndex)
  prettySrcLoc SrcLoc {..}
    = foldr (++) ""
      [ srcLocFile, ":"
      , show srcLocStartLine
      -- , ":"
      -- , show srcLocStartCol, " in "
      -- , srcLocPackage, ":", srcLocModule
      ]

here :: HasCallStack => String 
here = here' 1 

hereTrace :: HasCallStack => a -> a 
hereTrace = trace (here' 1) 

-- End ---------------------------------------------------------------
