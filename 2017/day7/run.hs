-- Incredibly ugly, should be fixed.
import Data.Maybe
import Data.Foldable
import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

data Disc = Disc String Int [String]
  deriving Show

discName (Disc n _ _) = n
discWeight (Disc _ w _) = w
discHolding (Disc _ _ h) = h


stripComma input =
  case reverse input of
    ',':input' -> reverse input'
    _ -> input

parse :: String -> [Disc]
parse = map parseDisc . lines

parseDisc line =
  let name:weight:rest = words line
      weight' = read . reverse . drop 1 .reverse . drop 1 $ weight
      holding =
        case rest of
          "->":rest' -> map stripComma rest'
          _ -> []
  in
    Disc name weight' holding

loeb x = fmap (\a -> a (loeb x)) x

part1 input =
  let held = Set.fromList $ concat $ map discHolding input
  in
    case filter (not . flip Set.member held) . map discName $ input of
      [name] -> name
      x -> error "More than one"

aggWeights :: Map String Disc -> Disc -> Int
aggWeights discs (Disc _ w h) =
  w + (sum . map (aggWeights discs . disc discs) $ h)

balanced :: Map String Disc -> Disc -> Bool
balanced discs d =
  (<= 1) $ length $ nub $ map (aggWeights discs . disc discs) $ discHolding d

disc :: Map String Disc -> String -> Disc
disc discs dname =
  case Map.lookup dname discs of
    Just d -> d
    Nothing -> error "Uh-oh"

part2 input =
  let discs = Map.fromList $ map (\d@(Disc n _ _) -> (n, d)) input
      parents = Map.fromList $ do
        Disc n _ h <- input
        child <- h
        pure (child, n)

      leafs = filter (null.discHolding) input
      candidates = candidates' $ map discName leafs
      candidates' [] = []
      candidates' nodes =
        let ps = nub . mapMaybe (flip Map.lookup parents) $ nodes
        in
          nodes ++ candidates' ps

      failing:_ = filter (not . balanced discs) $ map (disc discs) $ candidates
      failingChildrenWeights = map (\(Disc _ w h) -> (w, sum $ map (aggWeights discs.disc discs) h)) $ map (disc discs) $ discHolding failing
      failingChildrenTotals = map (uncurry (+)) failingChildrenWeights
      odd   = head $ filter (\x -> (== 1) . length . filter (== x) $ failingChildrenTotals) failingChildrenTotals
      other = head $ filter (\x -> (> 1) . length . filter (== x) $ failingChildrenTotals) failingChildrenTotals
      diff = odd - other
      oddWeight = fst . head $ filter (\(w, s) -> w+s == odd) failingChildrenWeights
      target = oddWeight - diff
  in
    target

main = do
  input <- parse <$> readFile "input.txt"
  putStrLn $ part1 input
  print $ part2 input
