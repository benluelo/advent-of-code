module Main where

import Data.Function
import Data.List
import qualified Data.Set as Set
import Data.Char (isUpper, isLower, isSpace)
import Text.Read
import System.Directory
import System.FilePath
import Debug.Trace

main :: IO ()
main = do
  fileNames <- listDirectory "../inputs/"
  let qualifiedFileNames = map ("../inputs/" ++) fileNames
  -- mapM_ print qualifiedFileNames
  fileContents <- mapM getDayAndInput qualifiedFileNames
  let
    final = fileContents
      & sortBy (\(a, _) (b, _) -> a `compare` b)
      & map runDay
  mapM_ print final 

getDayAndInput :: FilePath -> IO (Int, String)
getDayAndInput fp = do
  contents <- readFile fp
  return ((getDayFromPath fp), contents)

-- |split `day-N` into an integer `N`.
getDayFromPath :: FilePath -> Int
getDayFromPath fp = splitAt 4 (takeBaseName fp) & snd & read

data DayOutput = Unimplemented Int
  | DayOutput {
    day :: Int,
    part1 :: String,
    part2 :: String
  }

instance Show DayOutput where
  show (DayOutput {
    day = day,
    part1 = part1,
    part2 = part2
  }) = "day " ++ show (day) ++ "\n"
                ++ "part 1: " ++ part1 ++ "\n"
                ++ "part 2: " ++ part2 ++ "\n"
  show (Unimplemented day) = "day " ++ show (day) ++ " not yet implemented\n"

runDay :: (Int, String) -> DayOutput

runDay (1, input) = DayOutput {
  day = 1,
  part1 = input
    & parse
    & sortBy (flip compare)
    & maximum
    & show,
  part2 = input
    & parse
    & take 3
    & foldr (+) 0
    & show
} where
    parse :: String -> [Int] 
    parse input =
      lines input
      & map readMaybe
      & foldl (accumulate) []

    accumulate :: [Int] -> Maybe Int -> [Int]
    accumulate acc curr = case (acc, curr) of
      ([], Just curr) -> [curr]
      ([], Nothing) -> []
      (acc : finished, Just curr) -> acc + curr : finished
      (accumulated, Nothing) -> 0 : accumulated

runDay (3, input) = DayOutput {
  day = 3,
  part1 = input
    & parse
    & map (getDuplicate . (mapTuple $ Set.map (priority)))
    & sum
    & show,
  part2 = ""
} where
    parse :: String -> [(Set.Set Char, Set.Set Char)] 
    parse input = input
      & lines
      & map (
        mapTuple Set.fromList
        . mapTuple (filter $ not . isSpace)
        . splitInHalf
      )

    getDuplicate :: (Set.Set Int, Set.Set Int) -> Int
    getDuplicate (a, b) = case Set.toList $ Set.intersection a b of
      [x] -> x
      _ -> error ("bad input: " ++ show a ++ show b)

    mapTuple :: Show a => Show b => (a -> b) -> (a, a) -> (b, b)
    mapTuple f (a1, a2) = {- traceShow (a1, a2) -} (f a1, f a2)
    
    priority :: Char -> Int
    priority c = case c of
      c | isUpper c -> fromEnum c - 38
      c | isLower c -> fromEnum c - 96
      _ -> error ("bad input: '" ++ ['\'', c])

    splitInHalf :: [Char] -> ([Char], [Char])
    splitInHalf s = splitAt (length s `div` 2) s

runDay (dayNum, input) = Unimplemented dayNum
