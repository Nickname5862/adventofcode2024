import Data.List (permutations)
import Data.Maybe (maybeToList, mapMaybe)
import Debug.Trace (trace)
import Control.Monad (replicateM)

{--     PLAN VAN AANPAK

...
...

--}

-- types for clarity
type InputString = String
type TestValue = Int
type RemainingNumbers = [Int]
type Operators = [Int -> Int -> Int]

main = do
  input <- readFile "Day7.txt"
  let parsedInput = parseInput input
  putStr $ day7 parsedInput
  putStr "\n"

day7 :: [[Int]] -> String
day7 input = (show . sum) result where
    result = mapMaybe (\inputLine -> tryAllOperators (head inputLine) (tail inputLine)) input

-- try whether any combination of the operators results in the test-value
tryAllOperators :: TestValue -> RemainingNumbers -> Maybe TestValue
tryAllOperators testval remaining = if any (\o -> applySingleOperator remaining o == testval) (generateCombinations [(+), (*)] (length remaining)) then Just testval else Nothing

-- generates all combinations of grabbing a value from xs, n times
-- not exactly sure why this works yet
generateCombinations :: [a] -> Int -> [[a]]
generateCombinations xs n = replicateM n xs

-- applies a single list of Operators on the RemainingNumbers and returns the result
applySingleOperator :: RemainingNumbers -> Operators -> Int
applySingleOperator remaining operators = foldl (\r1 (r2, o) -> r1 `o` r2) (head remaining) (zip (tail remaining) operators)

parseInput :: InputString -> [[Int]]
parseInput input = map (\allWords -> map read $ init (head allWords) : tail allWords) allLines where
    allLines :: [[String]] = (map words . lines) input