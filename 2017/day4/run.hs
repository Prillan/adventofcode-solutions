import Data.List (sort, nub)

part1 input =
  length $ filter (\l -> length (nub l) == length l) $ map words input

part2 input =
  length $ filter (\l -> length (nub $ map sort l) == length l) $ map words input

main = do
  input <- lines <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
