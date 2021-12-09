import           Data.List (sortOn)
import qualified Data.HashMap.Strict as HashMap

occurBy :: (Int -> Int) -> [String] -> String
occurBy f =
  map (fst . head . sortOn (f . snd) . HashMap.toList)
  . foldr (zipWith update) empties
  where empties = repeat HashMap.empty
        update k m = HashMap.insertWith (+) k 1 m

part1 :: [String] -> String
part1 = occurBy negate

part2 :: [String] -> String
part2 = occurBy id

main :: IO ()
main = do
   input <- lines <$> readFile "input.txt"
   putStrLn (part1 input)
   putStrLn (part2 input)
