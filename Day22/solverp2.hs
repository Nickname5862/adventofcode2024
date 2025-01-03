import Data.Map (Map, fromList, toList, insert, keys, lookup, insertWith, empty, assocs, unionWith)
import Data.List (intercalate, transpose, groupBy, find, zip4, maximumBy)
import Prelude hiding (lookup)
import Data.Char (isDigit, digitToInt)
import Debug.Trace (traceShow, trace)
import Data.Bits (Bits(xor))


type Input = [Secret]
type Secret = Int
type Diff = Int -- the difference between Bananas
type Bananas = Int -- the 'ones digit'
type SeqOf4 = (Diff, Diff, Diff, Diff)


{-
Part 2 isn't much harder.
First, we must save all the intermediate secrets and get the last 'Bananas' out of them
Then we map it to the difference.

To actually calculate the result, we loop through all sets of 4 difference-values and store their count in a `Map`.
This way, we only have to loop through all the secrets once.
Since there are 19 possible values for each difference, this might create a Map of at most 19^4 ~= 130_000. That isn't so bad.
-}


main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsedInput = parseInput input
  print $ solve parsedInput


parseInput :: String -> Input
parseInput = map read . lines

solve :: Input -> (SeqOf4, Int)
solve secrets = maximumBy (\s1 s2 -> compare (snd s1) (snd s2)) (assocs seqMap) where
  allSecrets :: [[Secret]] 
  allSecrets = map (\secret -> scanl (\secret' f -> f secret') secret (replicate 2000 evolveSecret)) secrets
  inputDiffs :: [[(Diff, Bananas)]]
  inputDiffs = map (\secrets -> zipWith (\old new -> (lastDigit new - lastDigit old, lastDigit new)) secrets (drop 1 secrets)) allSecrets
  seqsOf4 :: [(Diff, Bananas)] -> [(SeqOf4, Bananas)]
  seqsOf4 l = let l' = map fst l in zip (zip4 l' (drop 1 l') (drop 2 l') (drop 3 l')) (map snd $ drop 3 l) -- go through a list, find each sequence of 4 in it, and take the last number of Bananas
  makeBananaMap :: [(SeqOf4, Bananas)] -> Map SeqOf4 Bananas
  makeBananaMap = foldr (uncurry (insertWith const)) empty -- save only the first sequence of 4 as key in the map, with the value being the number of Bananas
  seqMap :: Map SeqOf4 Bananas = foldl (\dmap monkey -> unionWith (+) dmap (makeBananaMap (seqsOf4 monkey))) empty inputDiffs -- go through each monkey and update the map using the previous function

evolveSecret :: Secret -> Secret
evolveSecret = evolve (* 2048) . evolve (`div` 32) . evolve (* 64) where
  evolve f secret = (prune . mix secret . f) secret
  mix = xor
  prune = (`mod` 16777216)

lastDigit :: Int -> Int
lastDigit = digitToInt . last . show





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