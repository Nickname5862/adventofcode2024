import Debug.Trace (trace)
main = do
  input <- readFile "Day2.txt"
  putStr (day2 input)
  putStr "\n"

day2 :: String -> String
day2 input = (show . sum) numbers where
    sentences :: [String] = lines input
    inputList :: [[Int]] = map (map read . words) sentences
    successes :: [Bool] = map (\a -> allIncreasingByLessThanThree a || allDecreasingByLessThanThree a) inputList
    numbers :: [Int] = map boolToInt successes

boolToInt :: Bool -> Int
boolToInt b | b = 1
            | otherwise = 0

allIncreasingByLessThanThree :: [Int] -> Bool
allIncreasingByLessThanThree l = all (\(a,b) -> b - a > 0 && b - a <= 3) pairs where
    pairs :: [(Int, Int)] = zip l (tail l)

allDecreasingByLessThanThree :: [Int] -> Bool
allDecreasingByLessThanThree l = all (\(a,b) -> a - b > 0 && a - b <= 3) pairs where
    pairs :: [(Int, Int)] = zip l (tail l)
