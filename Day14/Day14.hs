import Data.Map (Map, fromList, toList, insert, keys, lookup)
import Data.List (intercalate, transpose, groupBy, find)
import Prelude hiding (lookup)
import Data.Char (isDigit)


type Position = Coordinate
type Velocity = Coordinate
type Robot = (Position, Velocity)
type TileMap = [Robot]


main = do
  input <- readFile "Day14.txt"
  let parsedInput = parseInput input
  print $ day14 parsedInput

parseInput :: String -> [Robot]
parseInput input = let nums :: [[Int]] = map (\s -> parseNumsFromLine s [1,3,5,7]) (lines input) in map (\num -> ((head num, num !! 1), (num !! 2, num !! 3))) nums

day14 :: [Robot] -> Int
day14 = countRobotsInQuadrants . map (fpow 100 stepRobot)

stepRobot :: Robot -> Robot
stepRobot (pos, vel) = let newpos = (pos + vel) in ((fst newpos `mod` 101, snd newpos `mod` 103), vel)
-- Or, use `bimap`...? Either way, I am NOT defining `instance Integral Coordinate` for now...
-- or is it not that hard??

countRobotsInQuadrants :: [Robot] -> Int
countRobotsInQuadrants robots = product [len (<) (<), len (<) (>), len (>) (>), len (>) (<)] where
  len xCheck yCheck = length $ filter (\((x,y),_) -> x `xCheck` 50 && y `yCheck` 51) robots




------ HELPER FUNCTIONS




-- I can also make a generic function that tracks a specific syntax
-- e.g. `parseNumsFromPattern "p=*,* v=*,*\n". It would move through the line and capture anything until the next character appears
-- parseNumsFromPattern :: String -> String -> [String]
-- parseNumsFromPattern "*" input = [[read input]]
-- parseNumsFromPattern (p:pattern) (i:input) | p == i    = parseNumsFromPattern pattern input
--                                            | p == '*'  = parseNumsFromPattern pattern (i:input)
--                                            | otherwise = i : parseNumsFromPattern pattern input

-- useful function I found for repeating a function n times.
fpow :: Int -> (a -> a) -> a -> a
fpow n f = foldr (.) id $ replicate n f

-- parses digits and symbols like '-'
parseNumsFromLine :: String -> [Int] -> [Int]
parseNumsFromLine input = map (read . (groups !!)) where
  groups :: [String] = groupBy (\a b -> isNumChar a == isNumChar b) input
  isNumChar :: Char -> Bool = \c -> isDigit c || c == '-'

-- maps a two-dimensional input in the form of "AAA\nBBB" to a Map
-- where a coordinate like (1,0) would map to a char like 'A'
toCoordinateMap :: String -> Map Coordinate Char
toCoordinateMap = fromList . concat . zipWith f [0..] . lines where
    f y = zipWith (\x v -> ((x,y), v)) [0..]

showCoordinateMap :: Map Coordinate Char -> String
showCoordinateMap wh = intercalate "\n" groups where
    l :: [(Coordinate, Char)] = toList wh
    groups :: [String] = transpose $ map (map snd) $ groupBy (\(coord1,c1) (coord2,c2) -> fst coord1 == fst coord2) l

showCoordinateMapWithWalker :: (Coordinate, Char) -> Map Coordinate Char -> String
showCoordinateMapWithWalker (coord, c) wh = intercalate "\n" groups where
    l :: [(Coordinate, Char)] = toList (insert coord c wh)
    groups :: [String] = transpose $ map (map snd) $ groupBy (\(coord1,c1) (coord2,c2) -> fst coord1 == fst coord2) l

-- find an element in a two dimensional grid (assume there is only 1)
-- I assume you already used `toCoordinateMap` to make a Map
coordinateOfChar :: Map Coordinate Char -> Char -> Maybe Coordinate
coordinateOfChar m c = find (\key -> c `elem` lookup key m) (keys m)

-- Why is this not a default implementation?
type Coordinate = (Int, Int)

-- Allow for (+), (-) and (*) to be applied to coordinates
-- e.g. (1,3) * (4,5) = (4,15)
instance Num Coordinate where
  (+) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
  (*) (x1,y1) (x2,y2) = (x1 * x2, y1 * y2)
  abs (x1,y1) = (abs x1, abs y1)
  signum (x1, y1) = (signum x1, signum y1)
  fromInteger int = (fromInteger int, fromInteger int)
  negate (x1, x2) = (negate x1, negate x2)

-- To use `mod`, I need to make it an instance of Ord, Enum, and Real first. No thank you.