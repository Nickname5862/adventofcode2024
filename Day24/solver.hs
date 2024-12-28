{-# LANGUAGE LambdaCase #-}
import Data.Map (Map, fromList, toList, insert, keys, lookup, empty)
import Data.List (intercalate, transpose, groupBy, find, sortOn)
import Prelude hiding (lookup)
import Data.Char (isDigit)
import Control.Monad.State (State, MonadState (state), runState, foldM)
import Debug.Trace (traceShow)
import Data.Bits ((.&.), (.|.), xor)
import Data.Ord (Down(Down))

{-
You want to:
- Make a list of all gates which end in Z
- For each of these, check the requirements and recursively ask for their values
- However, instead of normal recursion, we can re-use information. We do this by passing in a Translation Map. This map will get larger every time we find a value

The steps then become: (example)
- Z00 requires the values of X00 and X01. Recurse to X00
- If X00 is already in the initial map, grab the value and return it and the unchanged map
- If, however, X00 is not already in the initial map, recurse on its requirements (it must have some, find it)
  Once the requirements are met, apply the operator and get a value V. Return V as well as add V to the Map.
- Z00 now has the value of X00. Now recurse on X01 passing along the updated Map.
- After Z00 is done, check for Z01 and pass on the updated Map with it.

In this way, we continuously pass along updated Maps and a value V. This perfectly follows the structure of a STATE MONAD.
This way, I can even work with loops as long as they are not required for any Z value!
I hope this extra effort is worth it. Either way, it is good practice! :)
-}

type Wire = String -- e.g. "Z01"
type WireValue = Int -- 0 or 1
data LogicGate = AND | OR | XOR deriving (Eq, Show)
type WireValueMap = Map Wire WireValue
type GateConnections = [(Wire, LogicGate, Wire, Wire)]
type Input = (WireValueMap, GateConnections)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsedInput = parseInput input
  print $ solve parsedInput

parseInput :: String -> Input
parseInput s = (inputMap, inputOperations) where
  (startValues, operations) :: (String, String) = let s' = splitOn "\n\n" s in (head s', s' !! 1)
  inputMap = fromList (map ((\ v -> (init (head v), read (v !! 1)) ) . words) (lines startValues))
  inputOperations = map ((\v -> (head v, parseGate (v !! 1), v !! 2, v !! 4)) . words) (lines operations)
  parseGate = \case
    "AND" -> AND
    "OR"  -> OR
    "XOR" -> XOR
    _     -> error "this gate should not occur"

-- filter the gate-connections starting with a 'z', and sort them on number
-- FIXME: `postscanl` is identical to my `tail . scanl` but I cannot use it. As the initial argument to `scanl` is 0 and therefor doesn't add to the value, I don't need `tail`
solve :: Input -> Int
solve (wireMap, connections) = (bitsToBinary . bits . filterZ) connections where
  filterZ :: GateConnections -> GateConnections = sortOn (Down . (\(_,_,_,wire) -> wire)) . filter (\(_,_,_,wire) -> head wire == 'z')
  bits :: GateConnections -> [WireValue] = map fst . scanl (\(_,wireMap) (_,_,_,wire) -> runState (getWirevalue connections wire) wireMap) (0, wireMap)

bitsToBinary :: [Int] -> Int
bitsToBinary = sum . zipWith (\i b -> b * 2^i) [0..] . reverse

-- given a list of gateConnections (input, never changing) and a single Wire to get the result for, get a Map and produce a result and an updated Map
getWirevalue :: GateConnections -> Wire -> State WireValueMap WireValue
getWirevalue connections wire = state $ \wireMap -> case lookup wire wireMap of
  Just wireVal' -> (wireVal', wireMap) -- a `map (,wireMap) v <|>` surely doesn't work since the second part is not a maybe. I could wrap it in such though Or `maybe` with a default
  Nothing -> case find (\(_,_,_,wire') -> wire' == wire) connections of
    Nothing -> error "if the value of a wire is not yet known in the map, it should always be the result of a gate-connection"
    Just (w1, g, w2, _) ->
      let (b1, w') = runState (getWirevalue connections w1) wireMap;
          (b2, w'') = runState (getWirevalue connections w2) w';
          gate = applyGate g
          in (b1 `gate` b2, w'')

applyGate :: LogicGate -> Int -> Int -> Int
applyGate AND b1 b2 = b1 .&. b2
applyGate OR b1 b2 = b1 .|. b2
applyGate XOR b1 b2 = b1 `xor` b2




------ HELPER FUNCTIONS




-- this can be intertwined in a series of function applications
-- e.g. `f . g . h` can become `f . traceId . g . traceId . h` to check the values coming out of `h` and `g` respectively
traceId :: Show a => a -> a
traceId v = traceShow v v

-- all possible combinations of the two lists
pairs :: [a] -> [b] -> [(a,b)]
pairs as bs = [(a,b) | a <- as, b <- bs]

-- A function like `splitOn` seems quite useful. `lines` can then be made to be `splitOn "\n"` and words is `splitOn " "`
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn splitter s = splitOn' [] s where
  splitOn' acc [] = [acc]
  splitOn' acc s = let l = length splitter in if take l s == splitter then acc : splitOn' [] (drop l s) else splitOn' (acc ++ take 1 s) (drop 1 s)
  -- FIXME: the `++` is a bit of shame

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