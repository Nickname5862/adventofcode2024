import Data.Map (Map, fromList, keys, lookup, empty, insert, elems, toList)
import Data.List (find, groupBy, intercalate, intersperse, transpose)
import Prelude hiding (lookup, Left, Right)
import Data.Maybe (isJust, fromJust, fromMaybe, isNothing)
import Control.Monad.State (State, MonadState (state), runState)
import Debug.Trace (trace)

type Robot = Coordinate
type Warehouse = Map Coordinate Char -- Char can be '#' = Wall, '[' or ']' = Box, '.' = Empty, and '@' = Robot
type GPS = Int
type Direction = Coordinate -- movement in the X and Y direction

-- ✅ WORKS
main :: IO ()
main = do
  inputMap <- readFile "Day15_map.txt"
  inputCommands <- readFile "Day15_commands.txt"
  let parsedInputMap = parseInput inputMap
  let parsedInputCommands = concat $ lines inputCommands
--   putStr $ showCoordinateMapWithWalker (snd parsedInputMap, '@') (fst parsedInputMap) -- `print` does not respect newlines
  print $ day15 parsedInputMap parsedInputCommands

-- ✅ WORKS
day15 :: (Warehouse, Robot) -> [Char] -> Int
day15 (warehouse, robot) moves = calculateGPS lastWarehouse where
    directions :: [Direction] = map dirToCoordinate moves
    -- lastWarehouse :: Warehouse = snd $ foldl (flip step) (robot, warehouse) directions -- repeat `step` until done
    lastWarehouse :: Warehouse = snd $ foldl (\state@(r, wh) dir -> trace ('\n' : showCoordinateMapWithWalker (r, '@') wh) step dir state) (robot, warehouse) directions -- repeat `step` until done

-- ✅ WORKS
parseInput :: String -> (Warehouse, Robot)
parseInput input = (insert robot '.' warehouse, robot) where -- replace the '@' with a '.'
    widenedInput :: String = concatMap widen input where
        widen '#' = "##"
        widen 'O' = "[]"
        widen '.' = ".."
        widen '@' = "@."
        widen '\n' = "\n"
        widen _   = error "This value can never appear in the input."
    warehouse :: Warehouse = toCoordinateMap widenedInput
    robot :: Robot = fromJust $ coordinateOfChar warehouse '@' -- fromJust is justified as there must be exactly one '@' in the warehouse

-- ✅ WORKS
dirToCoordinate :: Char -> Coordinate
dirToCoordinate '>' = (1,0)
dirToCoordinate '<' = (-1,0)
dirToCoordinate 'v' = (0,1)
dirToCoordinate '^' = (0,-1)
dirToCoordinate _   = error "A direction can only be '<', '>', '^', 'v'."

calculateGPS :: Warehouse -> GPS
calculateGPS warehouse = foldr (\(x,y) val -> val + (x + 100 * y)) 0 boxes where
    boxes :: [Coordinate] = filter (\key -> '[' `elem` lookup key warehouse) (keys warehouse)

-- update the position of the Robot and the map of the Warehouse
-- using a State is not worth the effort for me
-- NEW: moving left and right is very similar. Only moving up and down can give new side-effects
-- TODO: THIS IS NOT AS EASY AS I THOUGHT. [ AND ] SHOULD WORK TOGETHER!
step :: Direction -> (Robot, Warehouse) -> (Robot, Warehouse)
step dir (robot, warehouse) = (updatedRobot, updatedWarehouse) where
    stepSucceeds :: Maybe [(Coordinate, Char)] = step' (robot + dir) '.'
    step' :: Coordinate -> Char -> Maybe [(Coordinate, Char)] = \coord prevCell -> case lookup coord warehouse of
        Nothing  -> error "Robot can never walk out of the map." -- should not occur
        Just '[' -> (\a b -> ((coord, prevCell) : a) ++ b) <$> step' (coord + dir) '[' <*> step' (coord + dir + (0,1)) ']' -- check the value RIGHT of this, and move it in the same direction.
        Just ']' -> (\a b -> ((coord, prevCell) : a) ++ b) <$> step' (coord + dir) ']' <*> step' (coord + dir + (0,1)) '[' -- check the value LEFT of this, and move it in the same direction.
        Just '.' -> Just [(coord, prevCell)] -- new coordinate with content of previous cell
        Just '#' -> Nothing
        _        -> error "Robot can never encounter a cell with another value than '[', ']', '.' or '#'." -- should also not occur
    updatedRobot :: Robot = if isNothing stepSucceeds then robot else robot + dir
    updatedWarehouse :: Warehouse = foldr (\(k, a) wh -> insert k a wh) warehouse (fromMaybe [] stepSucceeds)





------ HELPER FUNCTIONS





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