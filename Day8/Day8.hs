import Data.Map (Map, empty, lookup, insert, elems)
import Data.List (subsequences, nub)
import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)

-- no need to construct the entire map, we only need the locations of the antennas
type Coordinate = (Int, Int)
type Antinode = Coordinate
type Antenna = Coordinate
type AntennasByChar = [[Antenna]]
type MapSize = Coordinate

-- get it over with, I'll declare them once only
instance Num Coordinate where
  (+) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
  (*) (x1,y1) (x2,y2) = (x1 * x2, y1 * y2)
  abs (x1,y1) = (abs x1, abs y1)
  signum (x1, y1) = (signum x1, signum y1)
  fromInteger int = (fromInteger int, fromInteger int)
  negate (x1, x2) = (negate x1, negate x2)

---

main = do
  input <- readFile "Day8.txt"
  let parsedInput = parseInput input
  print $ day8 parsedInput
  putStr "\n"

-- use of `head` is justified as we do not get an empty input
parseInput :: String -> (AntennasByChar, MapSize)
parseInput input = (result, (length (head grid), length grid)) where
  grid :: [[Char]] = lines input
  coordGrid :: [(Coordinate, Char)] = concat $ zipWith (\y subgrid -> zipWith (\x v -> ((x,y), v)) [0..] subgrid) [0..] grid
  antennaGrid :: [(Antenna, Char)] = filter (\(_,v) -> v /= '.') coordGrid
  -- keep concatting them to the result of a map, then take the `elems`.
  result :: AntennasByChar = elems $ foldr (\(antenna, c) m -> insert c (antenna : fromMaybe [] (lookup c m)) m) empty antennaGrid

-- given the AntennasByChar list, make all possible subsets of 2
-- then, given each pairs, return both antinodes of the antennas
-- then, concat, nub, etc.
day8 :: (AntennasByChar, MapSize) -> Int
day8 (antennas, mapsize) = (length . nub) antinodesWithinBorders where
  antennaPairs :: [(Antenna, Antenna)] = concatMap subsetsOfSize2 antennas
  antinodes :: [Antinode] = concatMap (\(a1, a2) -> let diff = a2 - a1 in [a1 - diff, a2 + diff]) antennaPairs
  antinodesWithinBorders :: [Antinode] = filter (\(x,y) -> x >= 0 && x < snd mapsize && y >= 0 && y < fst mapsize) antinodes

-- make subsets of size 2, then put them in a tuple format instead of a list format
-- `head` and `!! 1` are justified because we know the list has exactly length 2
-- FIXME: still, I'd like to prevent this if I can
subsetsOfSize2 :: [a] -> [(a,a)]
subsetsOfSize2 = map (\ls -> (head ls, ls !! 1)) . filter (\x -> length x == 2) . subsequences