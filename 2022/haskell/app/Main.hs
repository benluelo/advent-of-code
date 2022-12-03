{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

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

data Showable where Showable :: forall a. Show a => a -> Showable

instance Show Showable where
  show (Showable x) = show x

data DayOutput = DayOutput {
  day :: Integer,
  part1 :: Showable,
  part2 :: Showable
}

instance Show DayOutput where
  show output = "day " ++ show (day output) ++ "\n"
                ++ "part 1: " ++ show (part1 output) ++ "\n"
                ++ "part 2: " ++ show (part2 output) ++ "\n"

runDay :: (Integer, String) -> Showable
runDay (1, input) = Showable DayOutput {
  day = 1,
  part1 = parse input
    & sortBy (flip compare)
    & maximum
    & Showable,
  part2 = parse input
    & take 3
    & foldr (+) 0
    & Showable
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

runDay (dayNum, input) = Showable ("day " ++ show (dayNum) ++ " not yet implemented")
