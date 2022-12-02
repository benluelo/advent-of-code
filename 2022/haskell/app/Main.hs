module Main where

import Data.Function
import Text.Read

main :: IO ()
main = do
  fileLines <- readFile "../inputs/day-1.txt"
  lines fileLines & map readMaybe & foldl (accumulate) [] & maximum & print

accumulate :: [Integer] -> Maybe Integer -> [Integer]
accumulate acc curr = case (acc, curr) of
  ([], Just curr) -> [curr]
  ([], Nothing) -> []
  (acc : finished, Just curr) -> acc + curr : finished
  (accumulated, Nothing) -> 0 : accumulated
