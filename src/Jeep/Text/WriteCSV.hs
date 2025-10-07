{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Jeep.Text.WriteCSV where

-----------------------------------

import Control.Exception ( assert )
import Data.List ( all, intercalate )

-----------------------------------
  
toCSVRow :: Show a => ([a],[a]) -> String
toCSVRow (ins,out) = Data.List.intercalate "," (show <$> ins ++ out)

isValid :: [([a],[a])] -> Bool
isValid [] = True
isValid (xs:xss) = Data.List.all f xss where
  f xs' = length (fst xs') == length (fst xs)  &&
    length (snd xs') == length (snd xs)  

mkHeader :: Int -> Int -> String
mkHeader numInputs numOutputs = inputs ++ "," ++ outputs where
  inputs = intercalate "," $ (\i -> "i" ++ show i ) <$> [0..numInputs-1]
  outputs = intercalate "," $ (\i -> "o" ++ show i ) <$> [0..numOutputs-1]

toCSV :: Show a => [([a],[a])] -> String
toCSV xs = assert (isValid xs) result  where
  numInputs = ( length . fst . head ) xs
  numOutputs = ( length . snd . head ) xs
  header = mkHeader numInputs numOutputs
  body = unlines $ toCSVRow <$> xs
  result = header ++ "\n" ++ body

-----------------------------------

writeToFile :: Show a => String -> [([a],[a])] -> IO ()
writeToFile fname xs = do
  writeFile fname $ toCSV xs

-- End ---------------------------------------------------------------
