import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (maximum, minimum)

parseAll :: String -> [[Int]]
parseAll =
  map (map (read :: String -> Int))
  . map words
  . lines

part1 =
  let minMax r = (maximum r, minimum r)
  in
    sum . map (uncurry (-)) . map minMax

part2 input = sum $ do
  row <- input
  x <- row
  y <- row
  guard $ x /= y
  guard $ x `mod` y == 0
  pure $ x `div` y

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
