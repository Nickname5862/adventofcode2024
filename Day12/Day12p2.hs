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
type Sides = Int


{-
For counting the number of _sides_, it is identical to count the number of _corners_.
A corner be an outer corner or an inner corner, meaning it bends inwards or outwards.
For a flower F, neighboring flowers G and an empty space ., these situations can occur:

FG
G.

F.
..

F.
.G

All these scenario's can occur in any orientation, so they can be rotated 90, 180 and 270 degrees.
Counting, for all flowers F, how many of these scenario's occur, results in our answer.
It should be noted that one flower can find multiple corners, particularly when it is a one-patch region.

We can check for this by going over `allDirectionsEUC` (whose directions constantly turn right) and checking whether,
when considering entries 0 (up),1,2; entries 2,3,4; entries 4,5,6; and entries 6,7,8; are flowers, either result in:
1) TRUE, FALSE, TRUE, or
2) FALSE, FALSE, FALSE, or
3) FALSE, TRUE, FALSE
then count 1.
Which means, considering entries `take 3 . drop 0`, `take 3 . drop 2`, `take 3 . drop 4` and `take 3 . drop 6`
which is equal to ~ `map (\t -> take 3 . drop t) [0,2..6]`
-}


main :: IO ()
main = do
  input <- readFile "Day12.txt"
  let parsedInput = parseInput input
  print $ solve parsedInput

parseInput :: String -> Regions
parseInput = toCoordinateMap

-- Takes a full 11 seconds, but it does the trick!
solve :: Regions -> Int
solve cmap = (sum . map ((\gardenPlot -> area gardenPlot * sides gardenPlot) . map fst)) trueMap where
   trueMap = repeatFloodfill cmap (assocs cmap)
   area :: [Coordinate] -> Area = length
   -- we need `allDirectionsEUC ++ allDirectionsEUC` in order to get `take 3 . drop 6` to also contain the final (0,1). `allDirectionsEUC ++ [(0,1)]` would also work
   sides :: [Coordinate] -> Sides = \flowers -> sum $ map (\flower -> length $ filter (\t -> (map (+ flower) . take 3 . drop t) (allDirectionsEUC ++ allDirectionsEUC) `matchesCornerPatternIn` flowers) [0,2..6]) flowers
   matchesCornerPatternIn :: [Coordinate] -> [Coordinate] -> Bool = \dirs coords -> case map (`elem` coords) dirs of
    [True, False, True]   -> True -- inner corner
    [False, False, False] -> True -- outer corner
    [False, True, False]  -> True -- weird double corner
    _                     -> False

-- take a flower from `remaining` (initially all), apply `floodfill`, get `coords`, and recurse on `repeatFloodFill` with the same cmap and the difference of `remaining` and `coords`
repeatFloodfill :: Regions -> [Plant] -> [[Plant]]
repeatFloodfill cmap [] = []
repeatFloodfill cmap (plant:remaining) = let coords = floodfill cmap plant [] in coords : repeatFloodfill cmap (remaining \\ coords)

floodfill :: Regions -> Plant -> [Plant] -> [Plant]
floodfill cmap plant@(flower, t) acc | flower `notElem` keys cmap              = acc -- flower doesn't exist
                                     | maybe False (/= t) (lookup flower cmap) = acc -- flower does not have the same type
                                     | plant `elem` acc                        = acc -- flower is already processed
                                     | otherwise                               = foldl (\acc' dir -> floodfill cmap (flower + dir, t) acc') (plant:acc) allDirectionsMH





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

-- all ManHattan distances
allDirectionsMH :: [Coordinate]
allDirectionsMH = [(0,1), (1,0), (0,-1), (-1,0)]

allDirectionsEUC :: [Coordinate]
allDirectionsEUC = [(0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1)]

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