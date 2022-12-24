{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
import AoC
import AoC.Grid

import Control.Monad (guard)
import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

type N = Int

type SetGrid = HashSet (N, N)
type Bounds = ((N, N), (N, N))

getBounds :: SetGrid -> Bounds
getBounds g =
  let (rs, cs) = unzip $ HashSet.toList g
  in ((minimum rs, minimum cs), (maximum rs, maximum cs))

parseAll :: String -> SetGrid
parseAll =
  HashMap.keysSet
  . HashMap.filter (== '#')
  . parseMapGrid id

data Dir = N | S | W | E
  deriving (Show, Eq, Ord, Enum, Bounded)

next :: (Eq a, Enum a, Bounded a) => a -> a
next d | d == maxBound = minBound
       | otherwise     = succ d

setup :: SetGrid -> HashSet (V2 N)
setup = HashSet.map v2

dirv :: Dir -> V2 N
dirv =
  \case N -> v2 ( 0, -1)
        S -> v2 ( 0,  1)
        W -> v2 (-1,  0)
        E -> v2 ( 1,  0)

dirNeighbors :: Dir -> [V2 N]
dirNeighbors =
  \case N -> [ v2 (-1, -1)
             , v2 ( 0, -1)
             , v2 ( 1, -1)
             ]
        S -> [ v2 (-1, 1)
             , v2 ( 0, 1)
             , v2 ( 1, 1)
             ]
        W -> [ v2 (-1,  1)
             , v2 (-1,  0)
             , v2 (-1, -1)
             ]
        E -> [ v2 (1,  1)
             , v2 (1,  0)
             , v2 (1, -1)
             ]


freeNeighbors :: HashSet (V2 N) -> V2 N -> [V2 N]
freeNeighbors g pos =
  filter (not . (`HashSet.member` g))
  . map ((+ pos) . v2)
  $ [ ( 0,  1)
    , ( 0, -1)
    , ( 1,  0)
    , ( 1,  1)
    , ( 1, -1)
    , (-1,  0)
    , (-1,  1)
    , (-1, -1)
    ]
elfPropose :: HashSet (V2 N) -> V2 N -> Dir -> Maybe (Dir, V2 N)
elfPropose g pos dir = do
  let !free = freeNeighbors g pos
  guard $ length free /= 8
  let options = take 4 $ iterate next dir
  proposed <- find (\d -> all (`elem` free) (map (+ pos) (dirNeighbors d))) options
  pure (proposed, pos + dirv proposed)

propose :: HashSet (V2 N) -> Dir -> HashSet (V2 N)
propose g dir = go HashSet.empty HashMap.empty HashSet.empty (HashSet.toList g)
  where go !bad !seen !new =
          \case [] -> new
                !pos:rest ->
                  case elfPropose g pos dir of
                    Just (_, !pos')
                      | pos' `HashSet.member` bad ->
                          go bad
                             seen
                             (HashSet.insert pos new)
                             rest
                      | Just conflict <- seen HashMap.!? pos' ->
                          go (HashSet.insert pos' bad)
                             (HashMap.delete pos' seen)
                             (HashSet.insert pos
                               . HashSet.insert conflict
                               . HashSet.delete pos'
                               $ new)
                             rest
                      | otherwise ->
                          go bad
                             (HashMap.insert pos' pos seen)
                             (HashSet.insert pos' new)
                             rest
                    Nothing -> go bad seen (HashSet.insert pos new) rest

step :: (HashSet (V2 N), Dir) -> (HashSet (V2 N), Dir)
step (g, dir) =
  (propose g dir, next dir)

empty :: HashSet (V2 N) -> Int
empty g =
  let ((xmin, ymin), (xmax, ymax)) = getBounds $ HashSet.map (\(V2 t) -> t) g
  in
    (1 + xmax - xmin) * (1 + ymax - ymin) - fromIntegral (length g)

part1 :: SetGrid -> Int
part1 g =
  empty . fst $ iterate step (setup g, N) !! 10

part2 :: SetGrid -> Int
part2 g =
  fst $ fixpointiOn fst step (setup g, N)

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

smallMain :: IO ()
smallMain = main' "small.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
