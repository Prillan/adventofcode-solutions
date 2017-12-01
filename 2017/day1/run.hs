import           Control.Monad (guard)
import           Data.Char (isDigit)

parseAll = map (read :: String -> Int) . map pure . filter isDigit

part1 [] = 0
part1 (n:ns) = sum $ do
  (x, y) <- zip (n:ns) (ns ++ [n])
  guard $ x == y
  pure x

part2 [] = 0
part2 (n:ns) =
  let l = length ns + 1
      l2 = l `div` 2
      repeated = concat $ repeat (n:ns)
  in sum $ do
    (x, y) <- zip (n:ns) $ take l . drop l2 $ repeated
    guard $ x == y
    pure x

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
