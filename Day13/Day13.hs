import Data.Maybe (fromMaybe, mapMaybe)
import Data.Char (isDigit)
import Data.List (groupBy)


-- types for clarity
type IntTuple = (Int, Int)
type ClawMovement = IntTuple -- how much A and B move when pressing the buttons
type WinPosition = IntTuple -- where A and B should be to win
type ButtonPresses = IntTuple -- number of presses of button A and B respectively
type Clawmachine = (ClawMovement, ClawMovement, WinPosition)
type Cost = Int -- how many tokens did it cost to win


main = do
  input <- readFile "Day13_2.txt"
  let parsedInput = parseInput input
  print $ day13 parsedInput

day13 :: [Clawmachine] -> Cost
day13 clawmachines = sum $ mapMaybe getCheapestWin clawmachines

-- Attempt 1) trying all 100*100 options. Actually fast enough
-- getCheapestWin :: Clawmachine -> Maybe Cost
-- getCheapestWin (moveA, moveB, winPos) = minimumMaybe $ mapMaybe (\(buttonPresses :: (Int, Int)) -> if (fst buttonPresses, fst buttonPresses) * moveA + (snd buttonPresses, snd buttonPresses) * moveB == winPos then Just (fst buttonPresses * 3 + snd buttonPresses) else Nothing) [(a,b) | a <- [0 .. 100], b <- [0 .. 100]]

-- Attempt 2) try only 100 options by calculating B from A. Significantly faster
getCheapestWin :: Clawmachine -> Maybe Cost
getCheapestWin (moveA, moveB, winPos) = minimumMaybe $ mapMaybe (\a -> let b = (fst winPos - a * fst moveA) `div` fst moveB in if (a, a) * moveA + (b, b) * moveB == winPos then Just (a * 3 + b) else Nothing) [1 .. 100]

-- Look at this beauty! ⭐️
minimumMaybe :: (Foldable t, Ord a) => t a -> Maybe a
minimumMaybe = foldr (\a b -> Just $ maybe a (min a) b) Nothing

parseInput :: String -> [Clawmachine]
parseInput input = clawmachineRows rows where
    rows = lines input
    parseLine :: String -> IntTuple = \line -> let group = groupBy (\a b -> isDigit a == isDigit b) line in (read $ group !! 1, read $ group !! 3)
    clawmachineRows :: [String] -> [Clawmachine]
    clawmachineRows [] = []
    clawmachineRows (line1 : line2 : line3 : _ : xs) = (parseLine line1, parseLine line2, parseLine line3) : clawmachineRows xs -- every 4 rows until the end

---

-- get it over with, I'll declare them once only
instance Num IntTuple where
  (+) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
  (*) (x1,y1) (x2,y2) = (x1 * x2, y1 * y2)
  abs (x1,y1) = (abs x1, abs y1)
  signum (x1, y1) = (signum x1, signum y1)
  fromInteger int = (fromInteger int, fromInteger int)
  negate (x1, x2) = (negate x1, negate x2)







-- UNUSED!







{--
Simplest is just to try all possible combinations, take all the ones that work, and return the one that is the cheapest.
That does require 100 * 100 = 10_000 calculations for every of the ~100 claw machines we get as input.
We can make several optimizations however:

1) Finish earlier than 100. For X, go until `price / X`. If you go 94 up, and need to get to 8400, you already have 
   reached the maximum value at 89,3 times, without any Y. Going higher is a waste. Take the ceil(min(`price / X`, `price / Y`))
   for each button.

2) You don't need to generate all possible combinations and filter the shortest out at the end. You can try the combinations
   in order of cost! Since A costs 3 and B costs 1, you can go in order: (0A,1B), (0A,2B), (0A,3B), (1A,0B), (1A,1B)...
   similarly to checking all permutations for bits in a byte. Whenever you then find any solution, you know it is the cheapest.
   It is still worth using optimization 1) to check whether any claw machine is impossible.

3) You don't need to try all 100 for both (so 10_000). Try 100 for A, then use `div` to get one value out for B. If this works for X and Y, return it. Otherwise, Nothing.

None of this is enough for Part 2 though. 100_000_000 added before it, resulting in e.g. 10_000_000_012_748 is a lot. Even with an upperbound, there's no chance.
We need MORE.


BRUTE FORCE is not the solution. You gotta be smart.

Let's focus on X only.
a * 26 + b * 67 = 10000000012748.
Find all INTEGER values of a and b such that this equation holds. Yeah, this already is hard as fuck...
But if we find ONE solution, does that help us find others?

Maybe this actually becomes easier when we look at the full picture. We have TWO such equations


a * 26 + b * 67 = 10000000012748
a * 66 + b * 21 = 10000000012176

a = (10000000012748 - 67b) / 26
((10000000012748 - 67b) / 26) * 66 + 21b = 10000000012176
(10000000012748 - 67b) * (66 / 26) + 21b = 10000000012176
let C = (66 / 26) in
C(10000000012748 - 67b) + 21b = 10000000012176
10000000012748*C - 67b*C + 21b = 10000000012176
let D = 10000000012748*C in
let E = 67*C in
D - Eb + 21b = 10000000012176
D - (E+21)b = 10000000012176
b = (D - 10000000012176) / (E+21)

CAN WE JUST CALCULATE THIS SHIT??!!
Is there only one solution though?
> Seems like it.
Is there always a solution though?
> Seems like it. But not always an INTEGER solution, and sometimes with NEGATIVE values

Let's try it.

b = (D - 10000000012176) / (E+21)
b = (10000000012748 * (C-1)) / (67*C)
b = (10000000012748 * (66 / 26 - 1) / (67 * (66 / 26)) ~ 208050656077,0728
a = (10000000012748 - 67b) / 26 ~ een -151515151708,3030303

--}


-- generate an infinite list through recursion. Then use `take` or pattern matching to stop it
-- the pattern should follow [(0,1), (0,2), (0,3), (1,0), (1,1)...]
-- WAIT. This order will never produce e.g. (0,4) even though that might be the winning strategy
-- you gotta remember that (1,0) may have the same cost as (0,3) but not the same position of the claws!
-- generateButtonOrder :: [ButtonPresses]
-- generateButtonOrder = generateButtonOrder' 0 where
--     generateButtonOrder' i = (i,0) : (i,1) : (i,2) : (i,3) : generateButtonOrder' (i + 1)

-- you should actually produce a more complex sequence:
-- [(0,0), (0,1), (0,2), (1,0), (0,3), (1,1), (0,4), (1,2), (0,5), (1,3), (0,6), (2,0), (1,4), (0,7)...]
-- this is quite complex to make though... Interesting! Let's find out the structure:
{--
You have the normal list (0,0), (0,1) ... (0,7) ...
You interweave the pattern (1,0), (1,1) ... starting at index 3 (1*4 - 1) and every second one
You interweave the pattern (2,0), (2,1) ... starting at index 11 (3*4 - 1) and every third one
    This is (3*4 - 1) because you have two patterns interweaving, and 2 + the original 1 = 3.
    All use the pattern (x*4-1).
--}