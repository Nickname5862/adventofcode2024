import Data.Map (Map, lookup, fromList, keys)
import Prelude hiding (lookup)
import Data.Char (digitToInt)
import Data.List (nub)

type Coordinate = (Int, Int)
type Height = Int
type TopographicMap = Map Coordinate Height

main :: IO ()
main = do
  input <- readFile "Day10.txt"
  let parsedInput = parseInput input
  print $ solve parsedInput

parseInput :: String -> TopographicMap
parseInput input = fromList $ concat tmap where
  grid :: [[Height]] = (map (map digitToInt) . lines) input
  tmap :: [[(Coordinate, Height)]] = zipWith (\y subgrid -> zipWith (\x v -> ((x,y), v)) [0..] subgrid) [0..] grid

-- originally, the "score is the number of 9-height positions reachable from that trailhead via a hiking trail".
-- now, however, the "rating is the number of distinct hiking trails which begin at that trailhead".
-- the only difference is that now, a 9-height position may be reachable through multiple trails.
-- Luckily, this is a simple case of only REMOVING THE `nub` FROM THE `hike` FUNCTION!

solve :: TopographicMap -> Int
solve m = (sum . map (length . hike m 0)) trailheads where
  trailheads :: [Coordinate] = filter (\k -> 0 `elem` lookup k m) (keys m)

hike :: TopographicMap -> Height -> Coordinate -> [Coordinate]
hike _ 9 c = [c]
hike m h c = (concatMap (hike m (h+1) . sumCoords c) . filter (\dir -> (h+1) `elem` lookup (sumCoords c dir) m)) [(0,1), (1,0), (0,-1), (-1,0)]

sumCoords :: Coordinate -> Coordinate -> Coordinate
sumCoords (a,b) (c,d) = (a+c, b+d)