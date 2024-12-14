import Data.List (subsequences)

type Level = Int
type Report = [Level]
type IsReportSafe = Report -> Bool

-- We can do this:
-- 1) the easy way. Try removing all levels once, and if any results in a safe report, it is safe
-- 2) the hardes way. Check which intervals are bad, and only try removing this one (or perhaps the one before as well)
-- Right now, with this simple input and checking function, method 1 would work just fine.

main = do
    input <- readFile "Day2.txt"
    let parsedInput = parseInput input
    print $ day2 parsedInput

parseInput :: String -> [Report]
parseInput = map (map read . words) . lines

day2 :: [Report] -> Int
day2 = length . filter isSafe where
    isSafe :: IsReportSafe = \report ->
        let subs :: [Report] = filter (\v -> length v == length report - 1) $ subsequences report in
            any (\r -> allDecreasingByLessThanThree r || allIncreasingByLessThanThree r) subs

-- `drop 1` is the safe version of `tail`
allIncreasingByLessThanThree :: [Int] -> Bool
allIncreasingByLessThanThree l = all (\(a,b) -> (b - a > 0) && (b - a <= 3)) $ pairs l

allDecreasingByLessThanThree :: [Int] -> Bool
allDecreasingByLessThanThree l = all (\(a,b) -> (a - b > 0) && (a - b <= 3)) $ pairs l

pairs :: [a] -> [(a,a)]
pairs as = zip as (drop 1 as)
