import Data.Map (Map, empty, fromList, toList, insertWith)
import Data.List (subsequences, nub)

-- no need to construct the entire map, we only need the locations of the antennas
type Antinode = Coordinate
type Antenna = Coordinate
type AntennasByChar = [[Antenna]]
type MapSize = Coordinate

main :: IO ()
main = do
  input <- readFile "Day8.txt"
  let parsedInput = parseInput input
  print $ solve parsedInput

-- use of `head` is justified as we do not get an empty input
parseInput :: String -> (AntennasByChar, MapSize)
parseInput input = (antennas, gridSize) where
  gridSize = let l = lines input in (length (head l), length l)
  antennaGrid = (filter ((/= '.') . snd) . toList . toCoordinateMap) input
  antennas = (map snd . groupBy) antennaGrid

-- given the AntennasByChar list, make all possible subsets of 2
-- then, given each pairs, return both antinodes of the antennas
-- then, concat, nub, etc.
solve :: (AntennasByChar, MapSize) -> Int
solve (antennas, mapsize) = (length . nub) antinodes where
  antennaPairs :: [(Antenna, Antenna)] = let pairs = concatMap subsetsOfSize2 antennas in pairs ++ map (\(a,b) -> (b,a)) pairs -- should now be both-ways! Meaning, both (a,b) and (b,a) should be in here
  antinodes = concatMap (\(a1,a2) -> takeWhile antinodeWithinBorders $ map (\i -> a2 + (a2 - a1) * (i,i)) [0..]) antennaPairs
  antinodeWithinBorders :: Antinode -> Bool = \(x,y) -> x >= 0
                                                     && x < snd mapsize
                                                     && y >= 0
                                                     && y < fst mapsize

-- make subsets of size 2, then put them in a tuple format instead of a list format
-- `head` and `!! 1` are justified because we know the list has exactly length 2
-- FIXME: still, I'd like to prevent this if I can. But tuples require it
-- FIXME: also, it might not be the most efficient as we do generate all 2^n subsequences instead of those n(n-1)/2 of length 2
subsetsOfSize2 :: [a] -> [(a,a)]
subsetsOfSize2 = map (\ls -> (head ls, ls !! 1)) . filter ((== 2) . length) . subsequences





------ HELPER FUNCTIONS





-- Sort the `a`s by the value of `b`
-- FIXME: I should use `insertWith'` instead (strict, saves memory), but this is being replaced by a version I cannot download yet
-- FIXME: also, I wonder if `\a b -> head a :` isn't more efficient than `++`? How does `[a] ++ [b,c,d,e...]` actually unfold?
groupBy :: Ord b => [(a,b)] -> [(b,[a])]
groupBy = toList . foldr (\(a,b) m -> insertWith (++) b [a] m) empty

-- Or, a nicer looking method
groupBy' :: Ord b => (a -> b) -> [a] -> [(b,[a])]
groupBy' f = groupBy . map (\a -> (a, f a))

-- maps a two-dimensional input in the form of "AAA\nBBB" to a Map
-- where a coordinate like (1,0) would map to a char like 'A'
toCoordinateMap :: String -> Map Coordinate Char
toCoordinateMap = fromList . concat . zipWith f [0..] . lines where
    f y = zipWith (\x v -> ((x,y), v)) [0..]

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