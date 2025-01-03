import Data.Map (Map, fromList, toList, insert, keys, lookup, insertWith, empty)
import Data.List (intercalate, transpose, groupBy, find, isPrefixOf)
import Prelude hiding (lookup)
import Data.Char (isDigit)
import Debug.Trace (traceShow, trace)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromJust)


{-
I have a sneaky suspision that "find[ing] all the sets of three inter-connected computers" will be too much effort, but I am not sure.
> In hindsight, I was likely wrong.

I can turn the input into a `Map String [String]`, where every ComputerName is mapped to all ComputerNames that it connects to.
Note that I have to check both-ways: in 'XX-YY', XX connects to YY and YY to XX.
Then I check, for all ComputerNames that start with 't':
- Go over all pairs in its Map, and check whether they have each other in their map as well

I will find duplicates this way.
- I can use `nub` afterwards while sorting them in some specific ordering, but that is quite inefficient.
- I can, however, only consider directions one-way! In 'XX-YY', XX would point to YY but YY not to XX.
  This way, I will find XX as being in a 3-connection while not finding YY even though it is in the same connection.
  It might be efficient to get such a OnewayMap as well as a BothwayMap - the latter for checking whether pairs point to each other, saving some time.
  I will only do that if it appears that my implementation is too slow
  TODO: This does not work though! If AA-BB and BB-CC and CC-AA, you will not find this pair by looking at AA, BB, or CC.
  Either way, checking ALL of them and just dividing the number by 3 is only 3 times slower than my "advanced" solution.
  However, I must prevent, during pair-finding, that BB-CC is found as a pair and CC-BB as well. Otherwise, I will find even more duplicates.


---


It is very easy to find too many duplicates. Even generating "all" sets of three inter-connected computers is hard this way.
The easiest way is to reverse all `connections` such that a computer with a `t` is always on the left.
This worked flawlessly.
-}


type Input = Map Computer [Computer]
type Computer = String
type Set = (Computer, Computer, Computer)


main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsedInput = parseInput input
  print parsedInput
  print $ solve parsedInput


parseInput :: String -> Input
parseInput s = fromList $ clusterBy tLeftConnections where
  connections :: [(Computer, Computer)]
  connections = map (\l -> bimap (take 2) (take 2 . drop 3) (l,l)) (lines s)
  tLeftConnections :: [(Computer, Computer)]
  tLeftConnections = map (\(c1,c2) -> if "t" `isPrefixOf` c2 then (c1,c2) else (c2,c1)) connections -- make sure all `t`s are on the right when there are any. `clusterBy` will ensure these `t` will become the keys to the Map

solve :: Input -> Int
solve input = length tSets where
  tKeys :: [Computer]
  tKeys = filter ("t" `isPrefixOf`) (keys input) -- only find the sets of three when the first computer starts with a t
  tSets :: [Set]
  tSets = concatMap (\key -> let vs = fromJust $ lookup key input in map (\(c1, c2) -> (key, c1, c2)) $ filter (\(c1, c2) -> any (c2 `elem`) (lookup c1 input)) (pairs vs vs)) tKeys -- go over any pair in the values in the map and check whether they are connected to each other.





------ HELPER FUNCTIONS





-- this can be intertwined in a series of function applications
-- e.g. `f . g . h` can become `f . traceId . g . traceId . h` to check the values coming out of `h` and `g` respectively
traceId :: Show a => a -> a
traceId = traceIdSuffix ""

traceIdLn :: Show a => a -> a
traceIdLn = traceIdSuffix "\n"

traceIdSuffix :: Show a => String -> a -> a
traceIdSuffix s v = trace (s ++ show v) v

-- all possible combinations of the two lists
pairs :: [a] -> [b] -> [(a,b)]
pairs as bs = [(a,b) | a <- as, b <- bs]

-- A function like `splitOn` seems quite useful. `lines` can then be made to be `splitOn "\n"` and words is `splitOn " "`
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn splitter = splitOn' [] where
  splitOn' acc [] = [acc]
  splitOn' acc s = let l = length splitter in if take l s == splitter then acc : splitOn' [] (drop l s) else splitOn' (acc ++ take 1 s) (drop 1 s)
  -- FIXME: the `++` is a bit of a shame. I should be able to get rid of it (a `(:)` followed by a final `map reverse` works too, but is identically slow)

-- Sort the `a`s by the value of `b`
-- while `(++)` can be inefficient, it functions as no more than a single `(:)` here
-- FIXME: I should use `insertWith'` instead (strict, saves memory), but this is being replaced by a version I cannot download yet
clusterBy :: Ord b => [(a,b)] -> [(b,[a])]
clusterBy = toList . foldr (\(a,b) m -> insertWith (++) b [a] m) empty

-- Or, a nicer looking method
clusterBy' :: Ord b => (a -> b) -> [a] -> [(b,[a])]
clusterBy' f = clusterBy . map (\a -> (a, f a))

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

-- FIXME: I should be able to define this in terms of `showCOordinateMap` right?
showCoordinateMapWithWalker :: (Coordinate, Char) -> Map Coordinate Char -> String
showCoordinateMapWithWalker (coord, c) wh = intercalate "\n" groups where
    l :: [(Coordinate, Char)] = toList (insert coord c wh)
    groups :: [String] = transpose $ map (map snd) $ groupBy (\(coord1,_) (coord2,_) -> fst coord1 == fst coord2) l

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