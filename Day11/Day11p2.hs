import Data.Map (Map, fromList, toList, insert, keys, lookup, empty)
import Prelude hiding (lookup)
import Control.Monad.State (State, MonadState (state), runState)
import Debug.Trace (traceShow)

type Stone = Int
type Depth = Int
type Count = Int
type StoneMap = Map Stone [Count]

main = do
  input <- readFile "Day11.txt"
  let parsedInput = parseInput input
  putStr $ day11 75 parsedInput
  putStr "\n"

parseInput :: String -> [Stone]
parseInput = map read . words

day11 :: Int -> [Stone] -> String
day11 times = show . sum . map (last . fst) . tail . scanl (\(_,smap) stone -> runState (blink times stone) smap) ([], empty)

blink :: Depth -> Stone -> State StoneMap [Count]
blink 0     stone = state ([],)
blink depth stone = state $ \smap -> case lookup stone smap >>= takeMaybe depth of
  Just xs -> (xs, smap) -- the value could be found from the stone-map, so just copy the right section of it
  Nothing -> let newStones :: [Stone] = blinkingRules stone; -- either there was no entry in the map yet, or the entry did not have the right depth. Recurse manually
                 (stoneVals, smap') :: ([[Count]], StoneMap) =
                   let scanned = tail $ scanl (\(_,smap') nstone -> runState (blink (depth - 1) nstone) smap') ([],smap) newStones
                   in (map fst scanned, snd $ last scanned);
                 vals :: [Count] = length newStones : foldr (zipWith (+)) (repeat 0) stoneVals;
                 in (vals, insert stone vals smap') -- pass the state-map around until every new stone in the list has a value
                 -- add this new info to the map. Overwriting is not an issue, as this result is guaranteed to be of larger depth than the previous
                 -- and return the result by adding the sublists together: ([[1,2,3],[4,5,6]] would become [5,7,9])

blinkingRules :: Stone -> [Stone]
blinkingRules 0 = [1] -- rule 1
blinkingRules stone | even l    = [read $ take (l `div` 2) s, read $ drop (l `div` 2) s] -- rule 2
                    | otherwise = [stone * 2024] where -- rule 3
                      s = show stone
                      l = length s

-- a `maybe` function that applies `take` and returns `Nothing` when we try to take too many items
-- => when a repeated head fails, this returns nothing
takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe 0 as = Just []
takeMaybe _ [] = Nothing
takeMaybe v (a:as) = (a:) <$> takeMaybe (v-1) as

{--
TODO: There is a lot of repetition here! We can save the intermediate steps and reuse them.
How exactly would we do this though? Using a Map to save (StartingValue, Depth) -> NumberOfStones?
Since we go in DFS style, this will work wonders I think.
...
not for now.


LET'S DO IT!
Like Day 24, let's save all intermediate values in a Map. We then look in the map whether we already have the value, and if not, we recurse.
e.g.

> 0        []
> 1        [(0,1) -> 1]
> 2024     [(0,1) -> 1, (0,2) & (1,1) -> 1]
> 20,24    [(0,1) -> 1, (0,2) & (1,1) -> 1, (0,3) & (1,2) & (2024,1) -> 2]
... and this continues
Eventually we get
> 2,0,2,4
but this `0` will need to look at e.g. (0,72), which won't be in there for a long time. It would still be a waste to let it continue.

There must be an easier way. Or at least, a smarter one.

Maybe from the other way around. Go DFS and track them in reverse order. Meaning: only save values once you've reached 75!
Or, don't track the NUMBER of times, but track the actual RESULTS. This would be a severe increase in memory though.

YES!
--}

-- Unused, but will be useful in the future
-- (!?) :: [a] -> Int -> Maybe a
-- (!?) []     _ = Nothing
-- (!?) (a:_)  0 = Just a
-- (!?) (_:as) i = (!?) as (i-1)

-- this can be intertwined in a series of function applications
-- e.g. `f . g . h` can become `f . traceId . g . traceId . h` to check the values coming out of `h` and `g` respectively
traceId :: Show a => a -> a
traceId v = traceShow v v