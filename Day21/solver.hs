import Data.Map (Map, fromList, toList, insert, keys, lookup, insertWith, empty)
import Data.List (intercalate, transpose, groupBy, find)
import Prelude hiding (lookup)
import Data.Char (isDigit)
import Debug.Trace (traceShow)


type Input = String


main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsedInput = parseInput input
  print parsedInput
  print $ solve parsedInput


parseInput :: String -> Input
parseInput s = s

solve :: Input -> Int
solve i = 0

{-

I have discovered that:
- every code is independent (of course)
- every number is independent, as they all have to end in exactly one way: first robot on the number, second and third on A
- within that number, it is beneficial to use as many straight-lines: when the first robot is on A,
  it requires only one press to make the second robot make the first robot move in the current direction.
  So e.g. >>^^ is more beneficial than >^>^.
- but an exact plan I have not... yet. How to get the shortest path? Greedy?

<^ and ^< would be no difference for robot 1: it requires 2 moves
it also would be no difference for robot 2: it would need to move from A to < and ^, doesn't matter the order. Same number of moves: 9
it feels to me as if, as long as it goes in straight lines as often as possible, the robots do not care about the ordering in which they tackle the moves.
  at least, for 2 moves, that is. And they always need only 2 moves, as otherwise they would undo an action they did before (e.g. through >>^<)
So I can just take care of repeated moves (e.g. >>) and not care about the order of the moves furthermore. In other words: >>^^^ == ^^^>>
Cool!!  

Still, this is no easy feat. Walking from one move to another without passing a gap still requires some effort.
It should think about where to move next, a plan, and then move carefully to that

-}




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