import Data.Map (Map, fromList, lookup)
import Data.List (transpose, nub)
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)

type LeftList = [Int]
type RightList = [Int]

main = do
  input <- readFile "Day1.txt"
  let parsedInput = parseInput input
  print $ day1 parsedInput

day1 :: (LeftList, RightList) -> Int
day1 (leftList, rightList) = sum $ map (\left -> left * fromMaybe 0 (lookup left m)) leftList where
  m :: Map Int Int = fromList $ map (\left -> (left, (length . filter (left ==)) rightList)) (nub leftList)

-- unsafe `head` and `!! 1`, but justified given the input format
parseInput :: String -> (LeftList, RightList)
parseInput input = (head result, result !! 1) where
  result :: [[Int]] = (transpose . map (map read . words) . lines) input