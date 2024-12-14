import Data.List (transpose, zip4)
import Debug.Trace (trace)
import Data.Map (Map, fromList, keys, lookup)
import Prelude hiding (lookup)


type WordSearch = Map Coordinate Char

main = do
  input <- readFile "Day4.txt"
  let parsedInput = parseInput input
  print $ day4 parsedInput

parseInput :: String -> WordSearch
parseInput = toCoordinateMap

day4 :: WordSearch -> Int
day4 wordsearch = length $ filter formsMAS coordsOfAs where
    coordsOfAs :: [Coordinate] = filter (\key -> 'A' `elem` lookup key wordsearch) (keys wordsearch) -- find the coordinate of all occurances of the character 'A'    
    formsMAS :: Coordinate -> Bool = \coordinate ->
        let topRightOfCoord = lookup (coordinate + (1,1)) wordsearch
            botLeftOfCoord = lookup (coordinate + (-1,-1)) wordsearch
            topLeftOfCoord = lookup (coordinate + (-1,1)) wordsearch
            botRightOfCoord = lookup (coordinate + (1,-1)) wordsearch
            in (('M' `elem` topRightOfCoord && 'S' `elem` botLeftOfCoord) || ('S' `elem` topRightOfCoord && 'M' `elem` botLeftOfCoord)) &&
               (('M' `elem` topLeftOfCoord && 'S' `elem` botRightOfCoord) || ('S' `elem` topLeftOfCoord && 'M' `elem` botRightOfCoord))




------ HELPER FUNCTIONS




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