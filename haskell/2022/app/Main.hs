module Main where

import Data.Function
import Data.List
-- import qualified Data.List.Split as Split
import qualified Data.Set as Set
import Data.Char (isUpper, isLower, isSpace)
import Text.Read
import System.Directory
import System.FilePath
import Debug.Trace

main :: IO ()
main = do
  fileNames <- listDirectory "../../inputs/2022/"
  let qualifiedFileNames = map ("../../inputs/2022/" ++) fileNames
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
    & parse (\x -> map (splitInHalf) x)
    & show,
  part2 = input
    & parse (chunks3)
    & show
} where
    parse ::
    -- [String]    [[String]]
      ([[Char]] -> [[[Char]]]) -- ^Transform the input into a list of lists of chars. This has had all spaces removed.
      -> String -- ^The actual input. This will be split per line and passed to the callback function.
      -> Int
    parse doStuff input = doStuff (input
      & lines
      & map (filter $ not . isSpace))
      & map (
        map Set.fromList
        . map (map priority)
      )
      & map (findDuplicate)
      & sum

    findDuplicate :: [Set.Set Int] -> Int
    findDuplicate sets = case Set.toList $ foldr1 Set.intersection sets of
      [x] -> x
      _ -> error ("bad input: " ++ show sets)
    
    priority :: Char -> Int
    priority c = case c of
      c | isUpper c -> fromEnum c - 38
      c | isLower c -> fromEnum c - 96
      _ -> error ("bad input: '" ++ ['\'', c])

    splitInHalf :: String -> [[Char]]
    splitInHalf s =
      let
        (a, b) = splitAt (length s `div` 2) s
      in
        [a, b]

    chunks3 :: Show a => [a] -> [[a]]
    chunks3 [x, y, z] = [[x, y, z]]
    chunks3 (x:y:z:tail) = [x, y, z]:chunks3 tail
    chunks3 bad = error ("bad input: " ++ show (bad))

runDay (dayNum, input) = Unimplemented dayNum
