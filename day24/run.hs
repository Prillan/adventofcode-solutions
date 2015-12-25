import           Data.Aeson
import           Data.List (permutations, group, minimum, maximum, minimumBy, maximumBy, groupBy, (\\), sortOn)
import qualified Data.List as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Parsec
import Debug.Trace

goodSplits = filter isOk
           . splits
  where isOk (s, r, val, _, _) =  2 * val == sum r
                               && any ((== val) . sum) (L.subsequences r)

goodSplits' = filter isOk
            . splits
  where isOk (s, r, val, _, _) = 3 * val == sum r
                              && any subIsOk (L.subsequences r)
          where subIsOk s' = sum s' == 2 * val
                           && any ((== val) . sum) (L.subsequences $ r \\ s')

splits vals = map (\s -> (s, vals \\ s, sum s, product s, length s))
            . subsequences $ vals


--- vvv From stackoverflow
addoneall x xs = zipWith ((++) . (map (x:))) ([]:xs) (xs ++ [[]])
subsequences :: [a] -> [[a]]
subsequences = concat . foldr addoneall [[[]]]
--- ^^^

parseAll = map (read :: String -> Integer) . lines

part1 = sortOn (\(_,_,_,p,_) -> p)
      . head
      . groupBy (\(_,_,_,_,a) (_,_,_,_,b) -> a == b)
      . goodSplits
part2 = sortOn (\(_,_,_,p,_) -> p)
      . head
      . groupBy (\(_,_,_,_,a) (_,_,_,_,b) -> a == b)
      . goodSplits'

test = [1..5] ++ [7..11] :: [Integer]

main = do
   input <- parseAll <$> readFile "input.txt"
   print (head $ part1 input)
   print (head $ part2 input)
