import Data.List (elemIndex, intersect, nub)
import Data.List.Split (chunksOf)

score :: Char -> Int
score x =
  (+ 1)
  . sum
  . elemIndex x
  $ ['a'..'z'] ++ ['A'..'Z']

evenSplit :: [a] -> ([a], [a])
evenSplit x =
  let l = length x
  in
    splitAt (l `div` 2) x

parseAll :: String -> [String]
parseAll = lines

part1 :: [String] -> Int
part1 =
  sum
  . map (score . head . nub . uncurry intersect . evenSplit)

part2 :: [String] -> Int
part2 =
  sum
  . map (score . head . nub . foldl1 intersect)
  . chunksOf 3

main :: IO ()
main = main' "input.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
