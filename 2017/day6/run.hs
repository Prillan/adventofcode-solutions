import Data.Foldable
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


parse :: String -> Seq Int
parse = Seq.fromList . map read . words . head . lines

redistribute :: Int -> Int -> Int -> Seq Int -> Seq Int
redistribute size mi max memory =
  let (turns, rem) = max `divMod` size
      f i v
        | mi < i && i <= mi + rem = v + turns + 1
        | i <= mi + rem - size    = v + turns + 1
        | otherwise               = v + turns
  in Seq.mapWithIndex f . Seq.update mi 0 $ memory

part1 input =
  let (i, mem, seen) = until alreadySeen step (0, input, Set.empty)
  in
    i
  where alreadySeen (_, memory, s) = toList memory `Set.member` s
        size = Seq.length input
        step :: (Int, Seq Int, Set [Int]) -> (Int, Seq Int, Set [Int])
        step (i, memory, s) =
          let (mi, max) = maximumBy (comparing $ \(i, v) -> (v, -i))
                . zip [0..] . toList $ memory
              memory' = redistribute size mi max memory
          in
            (i + 1, memory', Set.insert (toList memory) s)

part2 input =
  let (i, mem, seen) = until alreadySeen step (0, input, Map.empty)
  in
    case Map.lookup (toList mem) seen of
      Just first -> i - first
      Nothing    -> -1 -- whoops
  where alreadySeen (_, memory, s) = toList memory `Map.member` s
        size = Seq.length input
        step :: (Int, Seq Int, Map [Int] Int) -> (Int, Seq Int, Map [Int] Int)
        step (i, memory, s) =
          let (mi, max) = maximumBy (comparing $ \(i, v) -> (v, -i))
                . zip [0..] . toList $ memory
              memory' = redistribute size mi max memory
          in
            (i + 1, memory', Map.insert (toList memory) i s)
main = do
  input <- parse <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
