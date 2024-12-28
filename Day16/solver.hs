import Data.Map (Map, fromList, toList, insert, keys, lookup)
import Data.List (intercalate, transpose, groupBy, find)
import Prelude hiding (lookup)
import Data.Char (isDigit)
import Control.Applicative (Alternative, (<|>))




{-
Even a bruteforce solution is not trivial, as it can easily get stuck in loops.
I could try Dijkstra's algorithm. Have every `.` keep track of the quickest path to it.
Every `.` needs to consider its direction as well.

The sad thing is that you need not _a_ path, but _the quickest_ path.
So you cannot use DFS but need some form of BFS.

I can just do bruteforce for now. But prevent cycles by looking them up in a Map.
-}


{-

`Alternative` is great! It functions sort-of like a general form of `maybe` build in the type!
Note that `Alternative` is not a type, but a type-class. A Maybe is e.g. an instance of Alternative.
An example:
> (Just 1) <|> (Just 2)
functions the same as
> Just $ maybe 2 id (Just 1)
Sure, you're stuck in Maybe world, but you get the idea. A backup option!
It works differently for lists. Here it concats instead, combining the different options.

It has a useful build-in functions:

1) `asum`, which is a `fold` function:
> foldr (<|>) empty
"For Maybe, asum finds the first Just x in the list and returns Nothing if there aren't any."
Really useful function indeed! I believe I could have used that for one of the exercises and probably used some weird `foldr (\a b -> if isJust b then b else a) Nothing`

2) `guard`, there it is! A complex function indeed. It evaluates a boolean, and
if false, it ignores (returns `empty`)
if true, it creates a dummy value (`pure ()`), not to be used, but to keep the flow going or smth.

-}

-- Alternative
-- (<|>)




------ HELPER FUNCTIONS




-- Look at this beauty! ⭐️
-- it is the same version of `minimum`, e.g. of a list
minimumMaybe :: (Foldable t, Ord a) => t a -> Maybe a
minimumMaybe = foldr (\a b -> Just $ maybe a (min a) b) Nothing

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