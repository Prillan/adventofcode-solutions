import Data.List (groupBy, transpose)

type Triangle = [Integer]

parseAll = map (map (read :: String -> Integer) . words) . lines

possible :: Triangle -> Bool
possible [x, y, z] =
  x + y > z &&
  x + z > y &&
  y + z > x

part1 = length . filter possible

example = [[1..3], [4..6], [7..9]]

grp n = map (map snd) . groupBy (\x y -> fst x == fst y) . zip (map (`div` n) [0..])

part2 = length . filter possible . concat . map transpose . grp 3

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
