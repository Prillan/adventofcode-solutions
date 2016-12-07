import           Data.List (sortOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

inputWidth :: Int
inputWidth = 8

occurBy f =
  map (fst . head . sortOn (f . snd) . Map.toList)
  . foldr (zipWith update) empties
  where empties = repeat Map.empty
        update k m = Map.insertWith (+) k 1 m

part1 = occurBy negate
part2 = occurBy id

main = do
   input <- lines <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
