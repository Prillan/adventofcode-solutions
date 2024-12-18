{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Grid

import Data.Bifunctor (first)
import Data.Foldable
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isJust)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

data Dir = U | D | L | R
  deriving Show

type Robot = (Int, Int)
type Input = (MapGrid Char, Robot, [Dir])

parseInstr :: String -> [Dir]
parseInstr = map f . filter (`elem` "^v<>")
  where f = \case '^' -> U
                  'v' -> D
                  '>' -> R
                  '<' -> L

parseAll :: String -> Input
parseAll input =
  let [grid, instr] = splitOn "\n\n" input
      g = parseMapGrid id grid
      Just (rpos, _) = find (\(_, v) -> v == '@') $ HashMap.toList g
      g' = HashMap.insert rpos '.' g
  in (g', rpos, parseInstr instr)

ray :: MapGrid Char -> (Int, Int) -> (Int, Int) -> [((Int, Int), Char)]
ray g (x, y) (dx, dy) =
  catMaybes . takeWhile isJust . map sequence $ map (\pos -> (pos, g HashMap.!? pos)) [ (x + i*dx, y + i*dy) | i <- [1..] ]

toDir :: Dir -> (Int, Int)
toDir = \case D -> ( 0,  1)
              U -> ( 0, -1)
              L -> (-1,  0)
              R -> ( 1,  0)

step :: MapGrid Char -> (Int, Int) -> Dir -> (MapGrid Char, (Int, Int))
step g robot instr =
  let d = toDir instr
      xs = ray g robot d
  in case (xs, dropWhile ((== 'O') . snd) xs) of
       ((robot', '.'):_, _) -> (g, robot')
       ((robot', 'O'):_, (empty, '.'):_) -> ( HashMap.insert empty 'O' $ HashMap.insert robot' '.' g
                                            , robot'
                                            )
       _ -> (g, robot)

gps1 :: MapGrid Char -> Int
gps1 =
  sum
  . map (\(x, y) -> x + 100 * y)
  . HashMap.keys
  . HashMap.filter (== 'O')

part1 :: Input -> Int
part1 (g, robot, instr) =
  let (final, _) = foldl' (uncurry step) (g, robot) instr
  in gps1 final

widen :: Input -> Input
widen (g, (rx, ry), instr) = (g', (2*rx, ry), instr)
  where g' = toMapGrid . map (concatMap f) $ fromMapGrid g
        f = \case
          '#' -> "##"
          'O' -> "[]"
          '.' -> ".."
          o -> error $ "what: " ++ show o

(|+|) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x, y) |+| (dx, dy) = (x + dx, y + dy)

shiftBox :: (Int, Int) -> [((Int, Int), Char)] -> MapGrid Char -> MapGrid Char
shiftBox d xs g =
  let boxes = takeWhile ((`elem` "[]") . snd) xs
      shifted = map (first (|+| d)) boxes
      empty = (fst $ head xs, '.')
  in HashMap.union (HashMap.fromList (empty:shifted)) g

collectAffected :: MapGrid Char
                -> (Int, Int)
                -> (Int, Int)
                -> Char
                -> Maybe [(Int, Int)]
collectAffected g d firstCell firstCellChar = go Set.empty initial
  where go :: Set (Int, Int) -> [(Int, Int)] -> Maybe [(Int, Int)]
        go seen = \case []     -> Just (toList seen)
                        (x:xs)
                          | x `Set.member` seen -> go seen (filter (not . (`Set.member` seen)) xs)
                          | otherwise ->
                            let !x' = (fst x + fst d, snd x + snd d) -- |+| d
                                !seen' = Set.insert x seen
                            in case g HashMap.!? x' of
                                 Just '#' -> Nothing
                                 Just '.' -> go seen' xs
                                 Just (!c) -> go seen' (x':extend x' c:xs)
                                 Nothing  -> error "Cannot happen"
        initial = [firstCell, extend firstCell firstCellChar]
        extend pos = \case '[' -> pos |+| ( 1, 0)
                           ']' -> pos |+| (-1, 0)

shiftAll :: MapGrid Char -> (Int, Int) -> [(Int, Int)] -> MapGrid Char
shiftAll g d cells =
  let shifted = zip (map (|+| d) cells) (map (g HashMap.!) cells) -- TODO: Redundant to read cells twice
      empties = map (, '.') cells
  in HashMap.unions [ HashMap.fromList shifted
                    , HashMap.fromList empties
                    , g
                    ]

step2 :: MapGrid Char -> Robot -> Dir -> (MapGrid Char, Robot)
step2 g robot instr = case instr of
                        U -> goY
                        D -> goY
                        L -> goX
                        R -> goX

  where d = toDir instr
        goX = let xs = ray g robot d
              in case (xs, dropWhile ((`elem` "[]") . snd) xs) of
                   ((robot', '.'):_, _) -> (g, robot')
                   ((robot', c):_, (_, '.'):_)
                     | c `elem` "[]" -> (shiftBox d xs g, robot')
                   _ -> (g, robot)
        goY =
          let robot' = robot |+| d
          in  case g HashMap.! robot' of
                c | c `elem` "[]"
                  , Just cells <- collectAffected g d robot' c
                  -> (shiftAll g d cells, robot')
                '.' ->  (g, robot')
                _ -> (g, robot)

printGrid :: (MapGrid Char, Robot) -> IO ()
printGrid (g, r) =
  putStrLn $ ppMapGrid id $ HashMap.insert r '@' g

gps2 :: MapGrid Char -> Int
gps2 =
  sum
  . map (\(x, y) -> x + 100 * y)
  . HashMap.keys
  . HashMap.filter (== '[')

part2 :: Input -> Int
part2 input =
  let (g, r, i) = widen input
      (final, _) = foldl' (uncurry step2) (g, r) i
  in gps2 final

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
