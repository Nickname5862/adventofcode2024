import Data.List (intercalate)
import Debug.Trace (trace)
import Data.Char (digitToInt)

type InputDiskmap = [Int]
data Fileblock = ID Int | Free -- ID number or Free space
newtype Diskspace = Disk [Fileblock]
newtype CompactDiskspace = CDisk [Int]
type Checksum = Int

main = do
  input <- readFile "Day9.txt"
  let parsedInput = parseInput input
  putStr $ day9 parsedInput
  putStr "\n"

day9 :: InputDiskmap -> String
day9 = show . calculateChecksum . toCompactDiskspace . diskmapToDiskspace

-- for easy debugging

instance Show Fileblock where
    show (ID i) = show i
    show Free     = "."
instance Show Diskspace where
    show (Disk diskspace) = intercalate "" $ map show diskspace
instance Show CompactDiskspace where
    show (CDisk diskspace) = intercalate "" $ map show diskspace

-- Helper functions

parseInput :: String -> InputDiskmap
parseInput = map digitToInt

fileblockIsID :: Fileblock -> Bool
fileblockIsID (ID _) = True
fileblockIsID Free   = False

diskmapToDiskspace :: InputDiskmap -> Diskspace
diskmapToDiskspace vs = Disk $ diskmapToDiskspace' vs 0 where
    diskmapToDiskspace' [] _ = []
    diskmapToDiskspace' (v:vs) i = replicate v (if even i then ID (i `div` 2) else Free) ++ diskmapToDiskspace' vs (i+1)

calculateChecksum :: CompactDiskspace -> Checksum
calculateChecksum (CDisk ls) = sum $ zipWith (*) [0..] ls

-- we use a reversed diskspace to prevent having to repeatedly look at the last value of the normal diskspace
toCompactDiskspace :: Diskspace -> CompactDiskspace
toCompactDiskspace (Disk disk) = (CDisk . take (length $ filter fileblockIsID disk) . toCompactDiskspace' disk) (reverse disk) where
    toCompactDiskspace' :: [Fileblock] -> [Fileblock] -> [Int]
    toCompactDiskspace' []    _  = []
    toCompactDiskspace' _    []  = [] -- should never happen
    toCompactDiskspace' (d:disk) (r:revDisk) = case d of
        (ID v) -> v : toCompactDiskspace' disk (r:revDisk)
        Free   -> case r of
            Free -> toCompactDiskspace' (d:disk) revDisk
            (ID v) -> v : toCompactDiskspace' disk revDisk