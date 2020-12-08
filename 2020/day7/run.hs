{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parse :: String -> ((String, String), [((String, String), Int)])
parse = go . words
  where go (c1:c2:_:_:"no":_) = ((c1, c2), [])
        go (c1:c2:_:_:rest) =
          ((c1, c2), readBags rest)
        readBags [] = []
        readBags (n:c1:c2:_:rest) = ((c1, c2), read n):readBags rest

parseAll =
  map parse . lines

rels = concatMap (\(p, cs) -> map ((,p) . fst) cs)

expand rules x = x:map snd (filter ((== x) . fst) rules)
findAll rules =
  let vals = iterate (nub . concatMap (expand rules)) [("shiny", "gold")]
  in fst . head . filter (uncurry (==)) $ zip (drop 1 vals) vals

part1 = length . drop 1 . findAll . rels

loeb :: Functor a => a (a x -> x) -> a x
loeb x = fmap (\a -> a (loeb x)) x

totalBags rules =
  let ruleMap = Map.fromList rules
      lookupCount m x =
        sum
        . map (\(c, n) -> n * (1 + lookupCount m c))
        $ ruleMap Map.! x
      ss = Map.fromList
        . map (\x -> (x, flip lookupCount x))
        . map fst
        $ rules
  in loeb ss

part2 input = totalBags input Map.! ("shiny", "gold")

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
