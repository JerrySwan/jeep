module Jeep.Data.Foldable where

-----------------------------------

import qualified Data.List.NonEmpty as NEL

-----------------------------------

runLengthEncode :: (Foldable f, Eq a) => f a -> [(a,Int)]
runLengthEncode = map (\x -> (NEL.head x, length x)) . NEL.group

runLengthEncodeBy :: (Foldable f, Eq a) => (a -> a -> Bool) -> f a -> [(a,Int)]
runLengthEncodeBy eq = map (\x -> (NEL.head x, length x)) . NEL.groupBy eq

-- End ---------------------------------------------------------------
