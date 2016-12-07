import           Data.List (sortOn)
import qualified Data.Map.Strict as Map

occurBy :: (Int -> Int) -> [String] -> String
occurBy f =
  map (fst . head . sortOn (f . snd) . Map.toList)
  . foldr (zipWith update) empties
  where empties = repeat Map.empty
        update k m = Map.insertWith (+) k 1 m

part1 :: [String] -> String
part1 = occurBy negate

part2 :: [String] -> String
part2 = occurBy id

main :: IO ()
main = do
   input <- lines <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
