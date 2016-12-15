{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
import           Control.Arrow ((&&&))
import           Data.Bifunctor (first, second)
import           Data.List (intercalate, isInfixOf, isSuffixOf, subsequences, sort, nub)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue
import           Data.Set (Set)
import qualified Data.Set as Set
import           Text.Megaparsec hiding (State)

type Element = Char

data Component a = Chip a | Gen a
  deriving (Ord, Eq, Show)

el :: Component Element -> Element
el (Gen x) = x
el (Chip x) = x

type GenLevel = Int
type ChipLevel = Int
type CompPair = (GenLevel, ChipLevel)
type Floor = Set (Component Element)
type State = (Int, [CompPair])

floorMap :: [Floor] -> Map (Component Element) Int
floorMap = Map.fromList . concat . zipWith (\i f -> map (,i) (Set.toList f)) [0..]

newState :: Map (Component Element) Int -> State
newState floors = (0, sort pairs)
  where comps = Set.fromList . map (el . fst) . Map.toList $ floors
        pairs = mapMaybe (\c -> (,) <$> Map.lookup (Gen c) floors
                                    <*> Map.lookup (Chip c) floors) $ Set.toList comps

-- printState :: State -> String
-- printState (el, fs) = unlines . map floor . zip [0..] $ fs
--   where floor (e, comps) =
--           (if el == e then "E" else " ")
--           ++ " : "
--           ++ intercalate " " (map show (Set.toList comps))

parseFloor :: String -> Floor
parseFloor = uncurry Set.union . (chips &&& gens) . filter ("ium" `isInfixOf`) . words
  where chips = Set.fromList . map (Chip . head)
                             . filter ("e" `isSuffixOf`)
        gens = Set.fromList . map (Gen . head) . filter ("ium" `isSuffixOf`)

parseAll = map parseFloor . lines

example :: [Floor]
example = [Set.fromList [Chip 'H', Chip 'L']
          ,Set.fromList [Gen 'H']
          ,Set.fromList [Gen 'L']
          ,Set.empty]

done :: State -> Bool
done (3, comps) = all (\(g, c) -> c == 3 && g == 3) comps
done _ = False

validState :: State -> Bool
validState (i, comps) =
  (0 <= i && i <= 3) &&
  (all check comps)
  where check :: CompPair -> Bool
        check (g, c) | g == c = True
        check (_, c) = all ((/= c) . fst) comps

subs :: [a] -> [[a]]
subs [] = []
subs xs = map pure xs ++ sub2 xs
  where
   sub2 :: [a] -> [[a]]
   sub2 [] = []
   sub2 (x:xs) = map (\x' -> [x, x']) xs ++ sub2 xs

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList

solve :: State -> (Int, State)
solve initial = solve' Set.empty (PQueue.singleton 0 (0, initial))

  where solve' :: Set State -> PQueue Int (Int, State) -> (Int, State)
        solve' visited queue =
          let Just ((steps, current), queue') = PQueue.minView queue
              !visited' = Set.insert current visited
              valid s = not (Set.member s visited')
              queue'' = foldr (uncurry PQueue.insert) queue'
                     . map (\(h, s) -> (h+steps+1, (steps+1, s)))
                     . filter (valid.snd) $ neighbours current
          in
            if done current
              then (steps, current)
              else solve' visited' queue''

        neighbours :: State -> [(Int, State)]
        neighbours (el, comppairs) =
          let candidates :: [Component Int]
              candidates = do
                (i, (g, c)) <- zip [0..] comppairs
                (if g == el then [Gen i] else []) ++
                  (if c == el then [Chip i] else [])

              groups = subs candidates

              modify :: Int -> Component Int -> [CompPair] -> [CompPair]
              modify el' c = zipWith modify' [0..]
                where (i, f) =
                        case c of
                          Gen x -> (x, first)
                          Chip x -> (x, second)
                      modify' j t
                        | i == j = f (const el') t
                        | otherwise = t

              moves = nub $ do
                el' <- [el-1, el+1]
                if (0 <= el' && el' <= 3) && (minimum (map (uncurry min) comppairs) <= el')
                  then do
                    g <- groups
                    pure (el', sort $ foldr (modify el') comppairs g)
                  else []
          in
            map (\s@(e, pairs) -> ((3-e) + sum (map ((6-) . uncurry (+)) pairs), s)) $ filter validState moves

part1 = solve . newState . floorMap
part2 x =
  let (e, comps) = newState (floorMap x)
  in
    solve (e, (0, 0):(0, 0):comps)

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
