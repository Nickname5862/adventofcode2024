
type Stone = Int

main = do
  input <- readFile "Day11.txt"
  let parsedInput = parseInput input
  putStr $ day11 parsedInput
  putStr "\n"

parseInput :: String -> [Stone]
parseInput = map read . words

day11 :: [Stone] -> String
day11 = show . length . fpow 25 blink

-- Helper functions

-- useful function I found for repeating a function n times.
fpow :: Int -> (a -> a) -> a -> a
fpow n f = foldr (.) id $ replicate n f

blinkingRules :: Stone -> [Stone]
blinkingRules i =
  if i == 0                                                      -- RULE 1
  then [1]
  else let iString = show i; l = length iString in if even l     -- RULE 2
    then [read $ take (l `div` 2) iString, read $ drop (l `div` 2) iString]
    else                                                         -- RULE 3 
      [i * 2024]

blink :: [Stone] -> [Stone]
blink = concatMap blinkingRules