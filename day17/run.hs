import           Data.Aeson
import           Data.List ( permutations
                           , group
                           , minimum
                           , maximum
                           , minimumBy
                           , maximumBy
                           , sort
                           , subsequences )
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Parsec

unsafeRight (Right x) = x

parseAll :: String -> [Int]
parseAll = -- map unsafeRight .
  map read . lines

part1 = length . filter ((==150) . sum) . subsequences

part2 input = length . filter ((== minsize) . length) $ exactly
  where exactly = filter ((==150) . sum) . subsequences $ input
        minsize = minimum . map (length) $ exactly

main = do
   input <- reverse . sort . parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
