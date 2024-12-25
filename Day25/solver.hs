{-# LANGUAGE LambdaCase #-}
import Data.Map (Map, fromList, toList, insert, keys, lookup)
import Data.List (intercalate, transpose, groupBy, find)
import Prelude hiding (lookup)
import Data.Char (isDigit)

type Pins = ([PinHeights], [PinHeights]) -- keys, locks
type PinHeights = [Int]
data SCell = Hashtag | Dot deriving Eq
type Schematics = [[SCell]] -- a temporary interpretation

main = do
  input <- readFile "input.txt"
  let parsedInput = parseInput input
  print $ solve parsedInput

parseInput :: String -> Pins
parseInput s = let schemas = (map (map (map parseChar)) . schematics) s in ((map pinheight . keySchematics) schemas, (map pinheight . lockSchematics) schemas) where
  schematics :: String -> [[String]] = map lines . splitOn "\n\n" -- split on newline, then split on double newline
  parseChar :: Char -> SCell = \case
    '#' -> Hashtag
    '.' -> Dot
    _   -> error "this symbol should not occur in the schematics."
  keySchematics :: [Schematics] -> [Schematics] = filter (all (== Hashtag) . last) -- last is unsafe but justified
  lockSchematics :: [Schematics] -> [Schematics] = filter (all (== Hashtag) . head) -- head is unsafe but justified
  pinheight :: Schematics -> PinHeights = map ((\v -> v - 1) . length . filter (== Hashtag)) . transpose

solve :: Pins -> Int
solve (keys, locks) = let maxHeight = 5 in length $ filter (\(key,lock) -> all (<= maxHeight) $ zipWith (+) key lock) (pairs keys locks)




------ HELPER FUNCTIONS




pairs :: [a] -> [b] -> [(a,b)]
pairs as bs = [(a,b) | a <- as, b <- bs]

-- A function like `splitOn` seems quite useful. `lines` can then be made to be `splitOn "\n"` and words is `splitOn " "`
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn splitter s = splitOn' [] s where
  splitOn' acc [] = [acc]
  splitOn' acc s = let l = length splitter in if take l s == splitter then acc : splitOn' [] (drop l s) else splitOn' (acc ++ take 1 s) (drop 1 s)

-- Example: `splitOn "CD" "ABCDEF"`:
-- See "AB", doesn't match, keep 'A'
-- See "BC", doesn't match, keep 'B'
-- See "CD", matches, don't keep and cause separation of some kind
-- See "EF", doesn't match, keep 'E'
-- See "F", doesn't match, keep 'F'
-- Done.

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