
-- An alternative way of solving this problem, even simpler without having to build the structure, using recursion.

type Stone = Int
type Depth = Int

main = do
  input <- readFile "Day11.txt"
  let parsedInput = parseInput input
  putStr $ day11 parsedInput
  putStr "\n"

parseInput :: String -> [Stone]
parseInput = map read . words

day11 :: [Stone] -> String
day11 = show . blink 25

-- Helper functions

blink :: Depth -> [Stone] -> Int
blink _ [] = 0
blink maxdepth (stone:stones) = blink' maxdepth stone + blink maxdepth stones where
  blink' :: Depth -> Stone -> Int
  blink' depth stone
    | depth <= 0 = 1
    -- RULE 1
    | stone == 0 = blink' (depth - 1) 1
    | otherwise = let s = show stone; l = length s in
      -- RULE 2
      if even l
        then blink' (depth - 1) (read $ take (l `div` 2) s) + blink' (depth - 1) (read $ drop (l `div` 2) s)
        -- RULE 3
        else blink' (depth - 1) (stone * 2024)