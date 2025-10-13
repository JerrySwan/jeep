module Jeep.Data.Foldable where

-----------------------------------

import Data.Foldable
import Data.Function

import qualified Data.List.NonEmpty as NEL

-----------------------------------

runLengthEncode :: (Foldable f, Eq a) => f a -> [(a,Int)]
runLengthEncode = map (\x -> (NEL.head x, length x)) . NEL.group

runLengthEncodeBy :: (Foldable f, Eq a) => (a -> a -> Bool) -> f a -> [(a,Int)]
runLengthEncodeBy eq = map (\x -> (NEL.head x, length x)) . NEL.groupBy eq

minimumOn :: (Foldable f, Ord b) => (a -> b) -> f a -> a
minimumOn f = minimumBy (compare `on` f)

maximumOn :: (Foldable f, Ord b) => (a -> b) -> f a -> a
maximumOn f = maximumBy (compare `on` f)

-- End ---------------------------------------------------------------
