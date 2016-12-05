import           Data.Aeson
import           Data.List (permutations, group, minimum, maximum, minimumBy, maximumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Parsec

data I = R | L | D | U
  deriving (Show, Read)

parseAll :: String -> [[I]]
parseAll = map (map ((read :: String -> I) . pure)) . lines

move pos R
  | pos `elem` [3, 6, 9] = pos
  | otherwise            = pos + 1
move pos L
  | pos `elem` [1, 4, 7] = pos
  | otherwise            = pos - 1
move pos D
  | pos `elem` [7, 8, 9] = pos
  | otherwise            = pos + 3
move pos U
  | pos `elem` [1, 2, 3] = pos
  | otherwise            = pos - 3

part1 = concat . map show . tail . scanl (foldl move) 5

move' pos R
  | pos `elem` [1, 4, 9, 12, 13] = pos
  | otherwise                    = pos + 1
move' pos L
  | pos `elem` [1, 2, 5, 10, 13] = pos
  | otherwise                    = pos - 1
move' pos D
  | pos == 1                 = 3
  | pos `elem` [2,3,4,6,7,8] = pos + 4
  | pos == 11                = 13
  | otherwise                = pos
move' pos U
  | pos == 3                    = 1
  | pos `elem` [10,11,12,6,7,8] = pos - 4
  | pos == 13                   = 11
  | otherwise                   = pos

showDigit x
  | 1 <= x && x <= 9 = show x
  | x == 10          = "A"
  | x == 11          = "B"
  | x == 12          = "C"
  | x == 13          = "D"

part2 = concat . map showDigit . tail . scanl (foldl move') 5

main = do
   input <- parseAll <$> readFile "input.txt"
   putStrLn (part1 input)
   putStrLn (part2 input)
