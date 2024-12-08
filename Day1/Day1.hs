import Data.List (transpose, sort)
import Debug.Trace (trace)
import Data.Type.Coercion (trans)

main = do
  input <- readFile "Day1.txt"
  putStr (day1 input)
  putStr ""

day1 :: String -> String
day1 input = show result where
  splitSentences :: [String] = lines input
  spacesRemoved :: [[Int]] = map removeSpaces splitSentences
  transposed :: [[Int]] = transpose spacesRemoved
  firstList :: [Int] = (sort . head) transposed
  secondList :: [Int] = (sort . head . tail) transposed
  result = sum (map (\(a, b) -> abs (a - b)) (zip firstList secondList))

removeSpaces :: String -> [Int]
removeSpaces s = map read (words s)

{--
- Obtain input
-- Remove spaces, get list of tuples
-- Transpose into two lists
- Sort both lists
- Zip them, do subtract and absolute
- Sum them

https://wiki.haskell.org/index.php?title=Debugging
trace :: String -> a -> a
--}