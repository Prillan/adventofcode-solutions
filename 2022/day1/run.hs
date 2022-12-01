import Data.List (sortOn)
import Data.List.Split (splitOn)

parseAll :: String -> [[Int]]
parseAll = map (map read) . splitOn [""] . lines

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . sortOn negate . map sum

main :: IO ()
main = main' "input.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
