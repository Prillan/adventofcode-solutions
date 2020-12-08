import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Cell = Tree | NoTree
  deriving (Show, Eq)

pp NoTree = '.'
pp Tree = '#'

ppCells n = map (take n . map pp)

parse '.' = NoTree
parse '#' = Tree

parseAll :: String -> [[Cell]]
parseAll =
  map (cycle . map parse)
  . lines

traverseStep :: (Int, Int) -> [[Cell]] ->  [[Cell]]
traverseStep (dx, dy) =
  drop dy
  . map (drop dx)

traverseAll :: (Int, Int) -> [[Cell]] -> [Cell]
traverseAll v =
  map (head . head)
  . takeWhile (not . null)
  . iterate (traverseStep v)

printCells n =
  putStrLn . unlines . ppCells n

countTrees v = length . filter (== Tree) . traverseAll v

part1 = countTrees (3, 1)

slopes =
  [ (1, 1)
  , (3, 1)
  , (5, 1)
  , (7, 1)
  , (1, 2) ]

part2 input =
  product $ map (flip countTrees input) slopes

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
