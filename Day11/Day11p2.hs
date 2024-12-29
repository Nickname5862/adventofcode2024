import Data.Map (Map, fromList, insert, lookup, empty)
import Prelude hiding (lookup)
import Control.Monad.State (State, MonadState (state), runState)

type Stone = Int
type Depth = Int
type Count = Int
type StoneMap = Map Stone [Count]

main :: IO ()
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
-- This does exist, namely as `takeExactMay`, but I can't download it yet (https://hackage.haskell.org/package/safe-0.3.21/docs/Safe-Exact.html#v:takeExactMay)
takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe 0 as = Just []
takeMaybe _ [] = Nothing
takeMaybe v (a:as) = (a:) <$> takeMaybe (v-1) as