import Data.Maybe (mapMaybe)
import Control.Monad (replicateM)

type InputString = String
type TestValue = Int
type RemainingNumbers = [Int]
type Operators = [Int -> Int -> Int]

main :: IO ()
main = do
  input <- readFile "Day7.txt"
  let parsedInput = parseInput input
  print $ solve parsedInput

parseInput :: InputString -> [[Int]]
parseInput input = map (\allWords -> map read $ init (head allWords) : tail allWords) allLines where
    allLines :: [[String]] = (map words . lines) input

solve :: [[Int]] -> Int
solve input = sum result where
    result = mapMaybe (\inputLine -> tryAllOperators (head inputLine) (tail inputLine)) input

-- try whether any combination of the operators results in the test-value
-- NOTE: this is really slow when many numbers are present, as it tries ALL combinations. Within a minute or two it has the answer though
tryAllOperators :: TestValue -> RemainingNumbers -> Maybe TestValue
tryAllOperators testval remaining = if any ((testval ==) . applySingleOperator remaining) (generateCombinations (length remaining) [(+), (*), (|||)])
                                    then Just testval
                                    else Nothing

-- concat operator. `(||)` was already taken by the 'or' operator
(|||) :: (Show a, Read a, Num a) => a -> a -> a
(|||) a b = read (show a ++ show b)

-- generates all combinations of grabbing a value from xs, n times
-- not exactly sure why this works yet
generateCombinations :: Int -> [a] -> [[a]]
generateCombinations = replicateM

-- applies a single list of Operators on the RemainingNumbers and returns the result
applySingleOperator :: RemainingNumbers -> Operators -> Int
applySingleOperator remaining operators = foldl (\r1 (o, r2) -> r1 `o` r2) (head remaining) (zip operators (tail remaining))
