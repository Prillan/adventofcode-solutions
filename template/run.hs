import           Data.List (permutations, group, minimum, maximum, minimumBy, maximumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Megaparsec

unsafeRight (Right x) = x

parseAll = -- map unsafeRight .
  map (parse _ "") . lines

part1 = id
part2 = id

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
--   print (part2 input)
