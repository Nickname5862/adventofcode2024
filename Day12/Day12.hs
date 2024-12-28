import Data.Map (Map, empty, fromList, toList, insert, keys, lookup)
import Data.List (intercalate, transpose, groupBy, find)
import Prelude hiding (lookup)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, isJust)


type PlantType = Char    -- e.g. 'A', 'B', or 'X'
type Regions = Map PlantType [Coordinate]
type Area = Int          -- => `(length . regions) char`
type Perimeter = Int     -- => for each coordinate in `lookup char regions`, sum all directions which are not in that list
type Price = Int


main = do
  input <- readFile "Day12.txt"
  let parsedInput = parseInput input
  print parsedInput
  -- print $ day12 parsedInput
  putStr "\n"

-- This is the hardest part, parsing it to such a structure
parseInput :: String -> Regions
parseInput input = empty where
  grid :: [[Char]] = lines input
  coordGrid :: [(Coordinate, Char)] = concat $ zipWith (\y subgrid -> zipWith (\x v -> ((x,y), v)) [0..] subgrid) [0..] grid
  -- now make regions by grouping all adjacent plants with the same plant-type. `groupBy` comes to mind, but only works for single lists.

-- We do not want to get duplicate regions, e.g. if we ask all the plants to group themselves with their neighbors
-- unless each plant checks whether it is not already part of a region. Whenever we add a plant to a region, we can remove
-- it from the list of plants left to process.

-- This is relatively easy.
day12 :: Regions -> Price
day12 regions = price where
  area :: PlantType -> Area = \ptype -> length $ lookup ptype regions
  perimeter :: PlantType -> Perimeter = \ptype ->
    let coords :: [Coordinate] = fromMaybe [] $ lookup ptype regions
        allDirections :: [Coordinate] = [(0,1), (1,0), (-1,0), (0,-1)] in
          length $ map (\coord -> length $ filter (\dir -> (coord + dir) `notElem` coords) allDirections) coords
  price :: Price = sum $ map (\ptype -> area ptype * perimeter ptype) (keys regions)


-- Helper functions



floodfillPlantgroups :: Map Coordinate Char -> [[Coordinate]]
floodfillPlantgroups plantMap = [] where
  inputCoordinates :: [[Coordinate]] = groupBy (\(_,y1) (_,y2) -> y1 == y2) $ keys plantMap -- group by Y-value
  groupedV1 :: [[[Coordinate]]] = map (groupBy (\c1 c2 -> lookup c1 plantMap == lookup c2 plantMap)) inputCoordinates
  groupedV2 :: [[Coordinate]] = [] -- 



{--
1) Parse the input. Divide the field into 'patches' or 'REGIONS' of plants.
   This requires looking at adjacently similar patches
   A function like `groupBy` springs to mind, but for two dimensions...

2) Decide the Area (`length`) and Perimeter (not too hard)
   Then multiply them, sum for each region, and done.


The hard part is that one char can occur multiple times, preventing a simple `Map PlantType [Coordinate]` structure.

A `[[Coordinate]]` structure is:
1) ~easy for counting the perimeter;
2) easy for counting the area
3) hard to setup

A `Map Coordinate Char` structure is:
1) hard to count the perimeter
2) hard to count the area
3) easy to setup

The only question: how do I build a `[[Coordinate]]` structure from a `Map Coordinate Char` structure?
Every cell has to belong to one list in the first structure.
My head wants to try every cell, and let every cell add every neighboring cells recursively like a `fill` until all are in the list.
Then you can also remove every cell you consider from the list of cells to consider.

So:
1) make a list of all coordinates to consider (all of them)
2) pick one coordinate. Check recursively for all neighbors to include them in its list, and remove them from the input list
   that's actually difficult, to `map` a list that you remove elements from...

A function like `groupBy` does a very well job... for one dimension.
The other dimension it does not help with.
However, we can use `groupBy` on the remaining lists right?

"AABB\nBAAB"
> `map groupBy`
[["AA", "BB"], ["B", "AA", "B"]
> merge with index, `groupBy overlapping index`
And remember, we'll do it on (COORD, CHAR) pairs, or only on (COORD) but with a map to CHAR.
Not entirely...
THIS IS DUMB!
Haskell isn't meant for 2D stuff...

You can:
1) Make a `Map Coordinate Char`
2) Reverse it into a `Map Char Coordinate`
If every planttype had a unique character, you'd be done. Now however:
3) Perform some adjacency checks.... the hard part

You can also, at the start, perform some floodfills to replace every character with a unique one... also hard

...to be continued

--}






------ HELPER FUNCTIONS





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