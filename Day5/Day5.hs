{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Prelude hiding (lookup)
import Data.Map (Map, lookup, fromList)
import Data.Maybe (isJust, fromJust, maybeToList)
import Data.List (elemIndices, findIndices)
import Debug.Trace (trace)

-- PLAN VAN AANPAK:

{--
1. Parse the input. For simplicity, save the PageOrderingRules in a separate file from the PagesToProduce.
   The PageOrderingRules are always 2 numbers followed by '|' and another 2 numbers.
   The PagesToProduce are lines consisting of 2 numbers followed by a ',' and another 2 numbers...
2. ...
--}

-- types for clarity
type Page = Int
type PageOrderingRules = Map Page [Page]
type PagesToProduce = [Page]

main = do
  inputRules <- readFile "Day5_rules.txt"
  inputPuzzle <- readFile "Day5_puzzle.txt"
  let parsedInputRules = parsePageOrderingRules inputRules
  let parsedInputPuzzle = map parsePagesToProduce (lines inputPuzzle)
  putStr (day5 parsedInputRules parsedInputPuzzle)
  putStr "\n"

day5 :: PageOrderingRules -> [PagesToProduce] -> String
day5 inputRules inputPuzzle = show result where
    succesfulPages :: [[Int]] = filter (isInOrder inputRules) inputPuzzle
    result :: Int = sum $ map getMiddleValue succesfulPages

-- checks whether a set of pages is actually in order
-- we map over all pages in the PagesToProduce, and produce a boolean whether all other numbers are not in their map or to their right
-- we use `and . maybeToList` to produce True both when no newPages are found (so, in case of `Nothing`) and when all newPages are to the right (so, in case of `Just True`)
isInOrder :: PageOrderingRules -> PagesToProduce -> Bool
isInOrder rules pages = all allToTheRight (zip pages [0..]) where
    allToTheRight :: (Page, Int) -> Bool = \(curPage, curIndex) -> and . maybeToList $ all (\newPage -> all (curIndex <) (elemIndices newPage pages)) <$> lookup curPage rules

-- assumes that there is at least one element in the list, which is always the case here
getMiddleValue :: [Int] -> Int
getMiddleValue ls = ls !! (length ls `div` 2)

parsePagesToProduce :: String -> PagesToProduce
parsePagesToProduce "" = [] -- base case
parsePagesToProduce input = (read . take 2) input : parsePagesToProduce (drop 3 input)

-- first, we just parse the input into a `[(Int, Int)]`.
-- then, we combine all second values into one list to product e.g. [(13, [14, 15])] instead of [(13, 14), (13, 15)]
-- we use a combination of `findIndices` and the gnarly `!!` operator to mimic a `findAll` function that sadly does not exist
parsePageOrderingRules :: String -> PageOrderingRules
parsePageOrderingRules input = fromList fullInput where
    basicInput :: [(Int, Int)] = (map (\v -> (read $ take 2 v, read $ drop 3 v)) . lines) input
    fullInput :: [(Int, [Int])] = map (\(a,b) -> (a, map (\i -> snd $ basicInput !! i) $ findIndices (\t -> fst t == a) basicInput)) basicInput
