{-# LANGUAGE TypeApplications #-}
import Data.Maybe
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

count e = length . filter (== e)

parseAll =
  map (read @Integer) . lines


l m = maybe 1 id . flip HashMap.lookup m

arrangements = flip l 0 . go HashMap.empty [] . reverse . sort . (0:)
  where go m _ [] = m
        go m [] (x:xs) = go (HashMap.insert x 1 m) [x] xs
        go m back (x:xs) =
          let options = takeWhile (<= (x + 3)) back
              v = sum $ map (l m) options
              m' = HashMap.insert x v m
          in go m' (x:back) xs

part1 input =
  let input' = 0:sort input
      diffs = zipWith (-) (drop 1 input') input'
  in (count 1 diffs) * (1 + count 3 diffs)
part2 = arrangements

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
