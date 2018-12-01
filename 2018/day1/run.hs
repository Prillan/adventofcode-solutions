import Data.Set (Set)
import qualified Data.Set as Set

part1 = sum
part2 = f Set.empty . scanl (+) 0 . concat . repeat
  where f s (i:is)
          | i `Set.member` s = i
          | otherwise = f (Set.insert i s) is

parseAll :: String -> [Int]
parseAll = map (read . f) . lines
  where f ('+':rest) = rest
        f rest = rest

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
