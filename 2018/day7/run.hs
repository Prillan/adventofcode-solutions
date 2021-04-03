import Data.Char (ord)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec (ErrorItem Char) String

type Step = (Char, Char)
type Graph = [(Char, Char)]

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

stepP :: Parser Step
stepP = do
  string "Step "
  from <- asciiChar
  string " must be finished before step "
  to <- asciiChar
  string " can begin."
  pure (from, to)


parseAll :: String -> [Step]
parseAll =
  map unsafeRight .
  map (parse stepP "") . lines

listPair :: (a, a) -> [a]
listPair (x, y) = [x, y]

allNodes :: Graph -> Set Char
allNodes = Set.fromList . concatMap listPair

graphElem :: Char -> Graph -> Bool
graphElem s = (s `elem`) . map snd

graphDelete :: Char -> Graph -> Graph
graphDelete s = filter ((s /=) . fst)

available :: Set Char -> Graph -> Set Char
available nodes g = Set.filter (not . (`graphElem` g)) nodes

part1 :: Graph -> String
part1 graph = go (allNodes graph) graph
  where go nodes g =
          case Set.minView (available nodes g) of
            Just (picked, _) ->
              picked:go (Set.delete picked nodes) (graphDelete picked g)
            Nothing -> []

work :: [(Char, Int)] -> [(Char, Int)]
work = map (\(c, t) -> (c, t - 1))

timeRequired :: Char -> Int
timeRequired c = ord c - ord 'A' + 1 + baseCost

baseCost :: Int
baseCost = 60

maxWorkers :: Int
maxWorkers = 5

pmap :: (a -> b) -> [a] -> [(a, b)]
pmap f = map (\a -> (a, f a))

alreadyAssigned :: [(Char, Int)] -> Char -> Bool
alreadyAssigned workers c = c `elem` (map fst workers)

part2 :: Graph -> Int
part2 graph = go 0 [] (allNodes graph) graph
  where go t workers nodes g
          -- No workers, no work. Done!
          | null workers && null nodes = t
          -- Some workers finished, pop them and their work.
          | any ((== 0) . snd) workers =
              let finished = map fst . filter ((== 0) . snd) $ workers
                  workers' = filter ((> 0) . snd) workers
                  nodes' = nodes Set.\\ (Set.fromList finished)
                  g' = foldl' (flip graphDelete) g finished
              in go t workers' nodes' g'
          -- Everyone's busy.
          | length workers == maxWorkers = go (t + 1) (work workers) nodes g
          -- There are some available workers now, find work for them.
          | otherwise =
              let availableWork = Set.toAscList (available nodes g)
                  freeWorkers = maxWorkers - length workers
                  newWorkers = take freeWorkers
                               . filter (not . alreadyAssigned workers)
                               $ availableWork
                  workers' = workers ++ pmap timeRequired newWorkers
              in
                go (t + 1) (work workers') nodes g

main :: IO ()
main = do
  input <- parseAll <$> readFile "input.txt"
  print (part1 input)
  print (part2 input)
