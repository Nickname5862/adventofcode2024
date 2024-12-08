import Data.List (elemIndices, elemIndex)
import Distribution.Compat.Prelude (readMaybe, catMaybes, mapMaybe)

main = do
  input <- readFile "Day3.txt"
  putStr (day3 input)
  putStr "\n"

day3 :: String -> String
day3 input = show result where
    indices :: [Int] = elemIndices 'm' input
    words :: [String] = map (takeNext12AfterIndex input) indices
    legalWords :: [[Int]] = mapMaybe performAllChecks words
    result :: Int = (sum . map product) legalWords

performAllChecks :: String -> Maybe [Int]
performAllChecks s = do
    v1 :: String <- checkForAndRemoveStartingMul s
    v2 :: String <- checkForAndRemoveEndingBracket v1
    v3 :: [String] <- checkForAndSplitOnComma v2
    let v4 :: [Maybe Int] = map checkForNumber v3
    v5 :: [Int] <- sequence v4 -- WTF is sequence?
    return v5

-- test 1
checkForAndRemoveStartingMul :: String -> Maybe String
checkForAndRemoveStartingMul s = if take 4 s == "mul(" then Just (drop 4 s) else Nothing

-- test 2
checkForAndRemoveEndingBracket :: String -> Maybe String
checkForAndRemoveEndingBracket s = elemIndex ')' s >>= \v -> Just (take v s)

-- test 3
checkForAndSplitOnComma :: String -> Maybe [String]
checkForAndSplitOnComma s = elemIndex ',' s >>= \v -> Just [take v s, drop (v + 1) s]

-- test 4
checkForNumber :: String -> Maybe Int
checkForNumber s = if length s <= 3 then readMaybe s else Nothing

-- 12 symbols is the longest legal word, e.g. "mul(333,555)"
takeNext12AfterIndex :: String -> Int -> String
takeNext12AfterIndex s i = drop i (take (i + 12) s)