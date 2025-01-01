import Data.Map (Map, empty, fromList, toList, insert, keys, lookup, assocs)
import Data.List (intercalate, transpose, groupBy, find, (\\))
import Prelude hiding (lookup)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (traceShow, trace)
import Control.Monad.State (State, MonadState (state))


type PlantType = Char
type Plant = (Coordinate, PlantType)
type Regions = Map Coordinate PlantType
type Area = Int
type Perimeter = Int


main :: IO ()
main = do
  input <- readFile "Day12.txt"
  let parsedInput = parseInput input
  print $ solve parsedInput

-- This is the hardest part, parsing it to such a structure
parseInput :: String -> Regions
parseInput = toCoordinateMap
-- now make regions by grouping all adjacent plants with the same plant-type. `groupBy` comes to mind, but only works for single lists.

-- Takes a full 5 seconds or smth, but it does the trick!!
solve :: Regions -> Int
solve cmap = (sum . map ((\gardenPlot -> area gardenPlot * perimeter gardenPlot) . map fst)) trueMap where
   trueMap = repeatFloodfill cmap (assocs cmap)
   area :: [Coordinate] -> Area = length
   perimeter :: [Coordinate] -> Perimeter = \coords -> sum $ map (\coord -> length $ filter (\dir -> (coord + dir) `notElem` coords) allDirections) coords 

-- take a flower from remaining (initially all), apply floodfill, get coords, recurse on `repeatFloodFill` with the same cmap and the intersection of remaining and coords
repeatFloodfill :: Regions -> [Plant] -> [[Plant]]
repeatFloodfill cmap [] = []
repeatFloodfill cmap (plant:remaining) = let coords = floodfill cmap plant [] in coords : repeatFloodfill cmap (remaining \\ coords)

floodfill :: Regions -> Plant -> [Plant] -> [Plant]
floodfill cmap plant@(flower, t) acc | flower `notElem` keys cmap              = acc -- flower doesn't exist
                                     | maybe False (/= t) (lookup flower cmap) = acc -- flower does not have the same type
                                     | plant `elem` acc                        = acc -- flower is already processed
                                     | otherwise                               = foldl (\acc' dir -> floodfill cmap (flower + dir, t) acc') (plant:acc) allDirections





------ HELPER FUNCTIONS





-- this can be intertwined in a series of function applications
-- e.g. `f . g . h` can become `f . traceId . g . traceId . h` to check the values coming out of `h` and `g` respectively
traceId :: Show a => a -> a
traceId v = traceShow v v

traceIdSuffix :: Show a => String -> a -> a
traceIdSuffix s v = trace (s ++ show v) v

traceIdLn :: Show a => a -> a
traceIdLn = traceIdSuffix "\n"

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

allDirections :: [Coordinate]
allDirections = [(0,1), (1,0), (0,-1), (-1,0)]

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