import Prelude hiding (Left, Right)
import Control.Monad.State (State, MonadState (state), runState)
import Data.List (elemIndex, findIndex, intercalate, transpose)
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (bimap)
import Data.Set (Set, insert, empty)
import Debug.Trace (trace)

-- helper types
newtype Area = Area [[Object]] deriving Eq -- 'Map' was confusing as there are many other things called Maps
data Object = Empty | Obstruction deriving Eq
type Coordinates = (Int, Int) -- X and Y coordinates. Note that Y coordinates go down instead of up
type PatrolPath = Set Coordinates
data Direction = Up | Right | Down | Left deriving (Show, Eq)
type Guard = (Coordinates, Direction)

instance Show Object where
    show Empty = "."
    show Obstruction = "#"
instance Show Area where
    show (Area area) = intercalate "\n" $ map (intercalate "" . map show) (transpose area)

main = do
  input <- readFile "Day6.txt"
  let parsedInput = parseInput input
  putStr $ day6 parsedInput empty
  putStr "\n"

-- apply `step area`, get an s = `State Guard Coordinates`
-- apply `runState s guard` to get a `(coordinates, newGuard)`
-- `insert` the old coordinates into the `path` to get a newPath
-- call `day6 (area, newGuard) (newPath)`
-- quit whenever the guard is out of bounds
day6 :: (Area, Guard) -> Set Coordinates -> String
day6 (area, guard) path = if isWithinBounds area $ fst guard
    then let (newCoordinates, newGuard) = runState (step area) guard in day6 (area, newGuard) (insert (fst guard) path)
    else show $ length path

-- Some helper functions

turnRight :: Direction -> Direction
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

directionToCoordinates :: Direction -> Coordinates
directionToCoordinates Up = (0,-1) -- note, reversed because Y is in reverse direction
directionToCoordinates Right = (1,0)
directionToCoordinates Down = (0,1) -- note, reversed because Y is in reverse direction
directionToCoordinates Left = (-1,0)

isWithinBounds :: Area -> Coordinates -> Bool
isWithinBounds (Area area) coordinates = let maxCoords = length area - 1 in not (fst coordinates < 0 || snd coordinates < 0 || fst coordinates > maxCoords || snd coordinates > maxCoords)

addCoordinates :: Coordinates -> Coordinates -> Coordinates
addCoordinates coordinates = bimap (fst coordinates +) (snd coordinates +)

objectInAreaAt :: Area -> Coordinates -> Object
objectInAreaAt (Area area) coordinates = area !! fst coordinates !! snd coordinates

-- if there is a block in front of you, turnRight
-- move one place forward (update Guard)
-- add the previous location to the path-state
-- NOTE: there are multiple ways to use a State here. You can either make the Guard or the ControlPath the state, or both.
-- I choose to make the Guard the State and return the previous coordinate to be added to the PatrolPath
step :: Area -> State Guard Coordinates
step area = state $ \guard -> updatePosition guard where
    updatePosition (oldCoordinates, oldDirection) = (oldCoordinates, (newCoordinates, newDirection)) where
        obstacleInFront :: Bool = let positionInFront = addCoordinates oldCoordinates (directionToCoordinates oldDirection) in
            isWithinBounds area positionInFront && (objectInAreaAt area positionInFront == Obstruction)
        newDirection = if obstacleInFront then turnRight oldDirection else oldDirection
        newCoordinates = let directionalCoordinates = directionToCoordinates newDirection in addCoordinates oldCoordinates directionalCoordinates

-- use of `fromJust` for the `guardLocation` is justified as there must be a guard in the puzzle
parseInput :: String -> (Area, Guard)
parseInput input = (Area $ transpose area, guardLocation) where
    rows :: [String] = lines input
    guardLocation :: Guard = let y = fromJust $ findIndex (isJust . elemIndex '^') rows in ((fromJust $ elemIndex '^' (rows !! y), y), Up)
    area :: [[Object]] = map (map parseSymbol) rows
    parseSymbol c = case c of
        '#' -> Obstruction
        _   -> Empty -- we expect only '.' and '^'
