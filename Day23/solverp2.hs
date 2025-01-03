import Data.Map (Map, fromList, toList, insert, keys, lookup, insertWith, empty)
import Data.List (intercalate, transpose, groupBy, find, isPrefixOf, maximumBy, sort)
import Prelude hiding (lookup)
import Data.Char (isDigit)
import Debug.Trace (traceShow, trace)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Maybe (fromJust)


{-
I see, the *maximum subclique* problem. You're lucky I forgot how to find this quickly
and refuse to look up any tips on how to complete these exercises...

We can find ALL cliques and take the maximum from that. And then I hope that this is quick enough.
> In hindsight, it is quick enough.
-}


type Input = Map Computer [Computer]
type Computer = String
type Set = (Computer, Computer, Computer)


main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsedInput = parseInput input
  print $ solve parsedInput


parseInput :: String -> Input
parseInput s = fromList $ clusterBy connections where
  connections :: [(Computer, Computer)]
  connections = bothways $ map (\l -> bimap (take 2) (take 2 . drop 3) (l,l)) (lines s)

solve :: Input -> String
solve input = (intercalate "," . sort . maximumBy (\l1 l2 -> compare (length l1) (length l2)) . allCliques input) (keys input)


-- take a key from the Map, and try:
-- 1) whether it connects to all in any of the cliques already produced
-- 2) if so, we duplicate this clique and add it to the new clique
-- 3) also, add the key itself as a clique too, to get it started
allCliques :: Map Computer [Computer] -> [Computer] -> [[Computer]]
allCliques _    [] = []
allCliques cmap (k:keys) = let cliques = allCliques cmap keys in [k] : update cliques ++ cliques where
  update = map (k:) . filter (all (\k' -> any (k `elem`) (lookup k' cmap))) -- filter the cliques that we can add ourselves to, and then map ourselves onto them

bothways :: [(a,a)] -> [(a,a)]
bothways [] = []
bothways ((a,b):as) = (a,b) : (b,a) : bothways as





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