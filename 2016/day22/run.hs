{-# LANGUAGE BangPatterns #-}
import           Control.Monad (guard)
import           Data.List ( intercalate
                           , maximum
                           , minimumBy
                           , subsequences)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Megaparsec hiding (State)
import Debug.Trace (trace)
data Node = Node { size :: {-# UNPACK #-} !Int
                 , used :: {-# UNPACK #-} !Int
                 , avail :: {-# UNPACK #-} !Int }
  deriving (Show, Eq, Ord)

numP = read <$> some digitChar
nodeP = Node <$> (some spaceChar *> numP <* string "T")
             <*> (some spaceChar *> numP <* string "T")
             <*> (some spaceChar *> numP <* string "T")
nodeNameP = (,) <$> (string "/dev/grid/node-x" *> numP)
                <*> (string "-y" *> numP)

lineP :: Parsec Dec String ((Int, Int), Node)
lineP = (,) <$> nodeNameP <*> nodeP

unsafeRight (Right x) = x

parseAll = Map.fromList . map unsafeRight .
  map (parse lineP "") . drop 2 . lines

viable :: Node -> Node -> Bool
viable a b =
  used a > 0 && avail b >= used a

part1 nodes = length $ do
  n1 <- Map.toList nodes
  n2 <- Map.toList nodes
  guard $ fst n1 /= fst n2
  guard $ viable (snd n1) (snd n2)
  pure $ (n1, n2)

data Reduced = NEmpty | NGoal | NBig | NNormal
  deriving (Show, Eq, Ord)
data State = State { goal :: {-# UNPACK #-} !(Int, Int)
                   , empty :: {-# UNPACK #-} !(Int, Int)
                   , nodes :: Map (Int, Int) Reduced }
  deriving (Show, Eq, Ord)

solve :: State -> (Int, State)
solve initial = solve' Set.empty (PQueue.singleton 0 (0, initial))
  where solve' :: Set State -> PQueue Int (Int, State) -> (Int, State)
        solve' visited queue =
          let Just ((!steps, !current), !queue') = PQueue.minView queue
              visited' = Set.insert current visited
              valid s = not (Set.member s visited')
              queue'' = foldr (uncurry PQueue.insert) queue'
                     . map (\s -> (h s + steps + 1, (steps + 1, s)))
                     . filter valid $ neighboursOf current
          in
            if done current
              then (steps, current)
              else  solve' visited' queue''

done s = goal s == (0, 0)

d (x0, y0) (x1, y1) =
  abs (x1 - x0) + abs (y1 - y0)

h s = 5 * (d (goal s) (0, 0))
    + d (empty s) (goal s)

viable' NNormal = True
viable' NGoal = True
viable' _ = False

neighboursOf :: State -> [State]
neighboursOf s = do
  let (x, y) = empty s
      Just en = Map.lookup (empty s) (nodes s)
  (x', y') <- [(x, y-1), (x-1, y), (x, y+1), (x+1, y)]
  Just bn <- pure $ Map.lookup (x', y') (nodes s)
  guard $ viable' bn
  let g' = if (x', y') == goal s
             then (x, y)
             else goal s
      e' = (x', y')
      m = Map.insert (x', y') en $
          Map.insert (x, y) bn $ nodes s
  pure $ State { goal = g'
               , empty = e'
               , nodes = m }

part2 = fst . solve . initialState

reduceGrid g = Map.mapWithKey f
  where f pos n
         | used n == 0   = NEmpty
         | pos == g      = NGoal
         | used n >= 100 = NBig
         | otherwise     = NNormal

initialState grid =
  let mx = maximum . map (fst . fst) . Map.toList $ grid
      g = (mx, 0)
      grid' = reduceGrid g grid
      (epos, _) = head . filter ((==NEmpty).snd) . Map.toList $ grid'
  in
    State { goal = g
          , empty = epos
          , nodes = grid' }

pstate s =
  putStrLn $ "Goal:  " ++ show (goal s)
        ++ "\nEmpty: " ++ show (empty s)
        ++ "\nh:     " ++ show (h s)

pgrid s =
  let xm = maximum . map (fst . fst) . Map.toList $ nodes s
      ym = maximum . map (snd . fst) . Map.toList $ nodes s
      c pos =
        let Just v = Map.lookup pos (nodes s)
        in case v of
             NEmpty -> '_'
             NNormal -> '.'
             NBig -> 'X'
             NGoal -> 'G'
  in
    intercalate "\n" [[c (x, y) | x <- [0..xm]] | y <- [0..ym]]

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
