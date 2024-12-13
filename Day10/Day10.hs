import Data.Map (Map, lookup, fromList, keys)
import Prelude hiding (lookup)
import Data.Char (digitToInt)
import Data.List (nub)

{--
I use a `Map` instead of a nested list because it has nicer properties.
- I can use the safe `lookup` instead of two unsafe `!!` applications.
- The speed is comparable:
  - O(log n) for the `Map` (https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html)
  - vs ~O(n) for `!!` (https://wiki.haskell.org/How_to_work_on_lists#Slower_operations)
--}

type Coordinate = (Int, Int)
type Height = Int
type TopographicMap = Map Coordinate Height

main = do
  input <- readFile "Day10.txt"
  let parsedInput = parseInput input
  putStr $ day10 parsedInput
  putStr "\n"

day10 :: TopographicMap -> String
day10 m = (show . sum . map (length . hike m 0)) trailheads where
  trailheads :: [Coordinate] = filter (\k -> 0 `elem` lookup k m) (keys m)

-- helper functions

parseInput :: String -> TopographicMap
parseInput input = fromList $ concat tmap where
  grid :: [[Height]] = (map (map digitToInt) . lines) input
  tmap :: [[(Coordinate, Height)]] = zipWith (\y subgrid -> zipWith (\x v -> ((x,y), v)) [0..] subgrid) [0..] grid

hike :: TopographicMap -> Height -> Coordinate -> [Coordinate]
hike _ 9 c = [c]
hike m h c = (nub . concatMap (hike m (h+1) . sumCoords c) . filter (\dir -> (h+1) `elem` lookup (sumCoords c dir) m)) [(0,1), (1,0), (0,-1), (-1,0)]

sumCoords :: Coordinate -> Coordinate -> Coordinate
sumCoords (a,b) (c,d) = (a+c, b+d)