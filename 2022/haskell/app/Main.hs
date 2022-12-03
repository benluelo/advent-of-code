module Main where

import Data.Function
import Data.List
import Text.Read
import System.Directory
import System.FilePath

main :: IO ()
main = do
  fileNames <- listDirectory "../inputs/"
  let qualifiedFileNames = map ("../inputs/" ++) fileNames 
  mapM_ print qualifiedFileNames
  fileContents <- mapM getDayAndInput qualifiedFileNames
  let final = fileContents & sortBy (\(a, _) (b, _) -> a `compare` b) & map runDay
  mapM_ print final 

getDayAndInput :: FilePath -> IO (Integer, String)
getDayAndInput fp = do
  contents <- readFile fp
  return ((getDayFromPath fp), contents)

-- |split `day-N` into an integer `N`.
getDayFromPath :: FilePath -> Integer
getDayFromPath fp = splitAt 4 (takeBaseName fp) & snd & read

data DayOutput = DayOutput {
  day :: Integer,
  part1 :: String,
  part2 :: String
}

instance Show DayOutput where
  show output = "day " ++ show (day output) ++ "\n"
                ++ "part 1: " ++ part1 output ++ "\n"
                ++ "part 2: " ++ part2 output ++ "\n"

runDay :: (Integer, String) -> DayOutput

runDay (1, input) = DayOutput {
  day = 1,
  part1 = parse input
    & sortBy (flip compare)
    & maximum
    & show,
  part2 = parse input
    & take 3
    & foldr (+) 0
    & show
} where
    parse :: String -> [Integer] 
    parse input =
      lines input
      & map readMaybe
      & foldl (accumulate) []

    accumulate :: [Integer] -> Maybe Integer -> [Integer]
    accumulate acc curr = case (acc, curr) of
      ([], Just curr) -> [curr]
      ([], Nothing) -> []
      (acc : finished, Just curr) -> acc + curr : finished
      (accumulated, Nothing) -> 0 : accumulated

runDay (dayNum, input) = undefined ("day " ++ show (dayNum) ++ " not yet implemented")
