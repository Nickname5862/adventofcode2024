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

    -- \ptype ->
    -- let coords :: [Coordinate] = fromMaybe [] $ lookup ptype regions in
    --       length $ map (\coord -> length $ filter (\dir -> (coord + dir) `notElem` coords) allDirections) coords

-- take a flower from remaining (initially all), apply floodfill, get coords, recurse on `repeatFloodFill` with the same cmap and the intersection of remaining and coords
repeatFloodfill :: Regions -> [Plant] -> [[Plant]]
repeatFloodfill cmap [] = []
repeatFloodfill cmap (plant:remaining) = let coords = floodfillF cmap plant [] in coords : repeatFloodfill cmap (remaining \\ coords)

-- repeat this floodfilling for every flower
-- TODO: Oh, dit is ook zo'n geval van "ik moet de accumulator doorgeven"...
-- Meaning, don't make it a `concatMap`, but a `scan`
-- And that means... State Monad again
-- floodfill :: Regions -> Plant -> [Coordinate] -> [Plant]
-- floodfill cmap (flower, t) acc | flower `notElem` keys cmap              = [] -- flower doesn't exist
--                                | maybe False (/= t) (lookup flower cmap) = [] -- flower does not have the same type
--                                | flower `elem` acc                       = [] -- flower is already processed
--                                | otherwise                               = (flower, t) : concatMap (\dir -> floodfill cmap (flower + dir, t) (flower : acc)) allDirections

floodfillF :: Regions -> Plant -> [Plant] -> [Plant]
floodfillF cmap plant@(flower, t) acc | flower `notElem` keys cmap              = acc -- flower doesn't exist
                                      | maybe False (/= t) (lookup flower cmap) = acc -- flower does not have the same type
                                      | plant `elem` acc                        = acc -- flower is already processed
                                      | otherwise                               = foldl (\acc' dir -> floodfillF cmap (flower + dir, t) acc') (plant:acc) allDirections

-- floodfill should:
-- return a list of Plants
-- recurse on the neighbors, one by one, each being passed a [Coordinate] and returning a new [Coordinate]
-- I might not even need a State! Maybe I just needed to replace `concatMap` with `foldl`!
-- floodfillS :: Regions -> Plant -> State [Coordinate] [Plant]
-- floodfillS cmap (flower, t) | flower `notElem` keys cmap              = state ([],) -- flower doesn't exist
--                             | maybe False (/= t) (lookup flower cmap) = state ([],) -- flower does not have the same type
--                             | otherwise                               = state $ \handledCoords -> if flower `elem` handledCoords -- flower is already processed
--                                                                                            then ([], handledCoords)
--                                                                                            else foldl (\(p,c) dir -> ) ([], handledCoords) allDirections




-- Helper functions




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

-------------------------------------------------------------------------------------------

Let's pick it back up! I am smarter now >:)

The only goal:
- given the input, create a [[Coordinate]], with each item in the outer list being a list of garden plots
- you can then count the perimeter by summing over all items and checking whether the neighbors are also in the list;
- you can then count the area by taking the length;
- the hard part? a character may represent multiple regions, preventing my `Map reversing` strategy from working

=> combine all of the same character, then split them up using non-adjacency checks
=> or combine them using adjacency checks
both are hard!

I would love to use `groupBy`, be it a custom version of it
- Make a Map Coordinate Char
- Then make a groupBy that groups adjacent chars by x-coordinate, into a [[(Coordinate, Char)]]
- then, somehow, for each pair (pairwise), group all adjacent lists together. That is not trivial.

o We have a `Map Coordinate Bool` to check whether you're done already
o We have a `Map Coordinate Char` to start with
o We make a Map Char [[Coordinate]]
I can also try floodfill to make a Map Char [[Coordinate]]:
- Make a Map Coordinate Char
- for each coordinate, check if it already exists in the floodfill
  if so, continue to the next coordinate
  if not, ......this is getting complicated



For every (coordinate,char) pair, check if it exists in a floodfill with the same char already.
  If not, let it flood to all neighbors, adding them to a list.
  In the end, return this list and put it in the floodfill map.

Does not require passing the Map around. Only passing the list around, which we append constantly.
That requires some `fold` behavior, where the accumulator which is returned is also passed around.
But I want to use recursion. I would have to pass the accumulator with it.

--}




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