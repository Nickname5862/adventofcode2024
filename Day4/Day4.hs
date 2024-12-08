import Data.List (transpose, zip4)
import Debug.Trace (trace)

main = do
  input <- readFile "Day4.txt"
  putStr (day4 input)
  putStr "\n"

day4 :: String -> String
day4 s = (show . sum . map numberOfOccurancesOfXmas) allLettersets where
    input :: Letterset = lines s
    allLettersets :: Letterset = horizontalLetters input ++ verticalLetters input ++ diagonalLetters input


type Letterset = [String]

-- normal and reverse
horizontalLetters :: Letterset -> Letterset
horizontalLetters s = s ++ map reverse s

verticalLetters :: Letterset -> Letterset
verticalLetters s = transpose s ++ (map reverse . transpose) s

-- remember: both downright and topright
diagonalLetters :: Letterset -> Letterset
diagonalLetters s = diagonals s ++ (map reverse . diagonals) s ++ (diagonals . transpose . map reverse) s ++ (map reverse . diagonals . transpose . map reverse) s

numberOfOccurancesOfXmas :: String -> Int
numberOfOccurancesOfXmas l = sum (map f quadZip) where
    quadZip :: [(Char, Char, Char, Char)] = zip4 l (tail l) (tail (tail l)) ((tail . tail . tail) l)
    f ('X', 'M', 'A', 'S') = 1
    f _                    = 0


-- copied from https://stackoverflow.com/questions/32465776/getting-all-the-diagonals-of-a-matrix-in-haskell
diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]