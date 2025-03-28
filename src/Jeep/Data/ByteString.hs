module Jeep.Data.ByteString where

-----------------------------------

import Data.Hex

import Jeep.Data.List

import qualified Data.ByteString               as B
-- import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
-- import qualified Data.ByteString.Lazy.Internal as BLI

-----------------------------------

showHex :: B.ByteString -> String
showHex bs = filter (/= '"') $ unwords $ (\xs -> "0x" ++ show xs)
  <$> chunks 2 (B.unpack (hex bs))

showHexLazy :: BL.ByteString -> String
showHexLazy b = showHex (BL.toStrict b)

-- End ---------------------------------------------------------------
