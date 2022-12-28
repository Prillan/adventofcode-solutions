{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Search (bfs_)

import Control.Monad (guard, forM_)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Ix (index, rangeSize)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Valve = (Char, Char)
type N = Int

type Parser a = Parsec Void String a

type IxValve = Int

ix :: Valve -> IxValve
ix = index bounds

bounds :: ((Char, Char), (Char, Char))
bounds = (('A', 'A'), ('Z', 'Z'))

numP :: Num a => Parser a
numP = fromInteger . read <$> some digitChar

nameP :: Parser Valve
nameP = (,) <$> upperChar <*> upperChar

valveP :: Parser (Valve, (Int, [Valve]))
valveP = do
  valve <- string "Valve " *> nameP
  rate <- string " has flow rate=" *> numP <* string ";"
  _ <- choice [ string " tunnels lead to valves "
              , string " tunnel leads to valve "
              ]
  valves <- sepBy1 nameP (string ", ")
  pure (valve, (rate, valves))

parseAll :: String -> IntMap (N, [IxValve])
parseAll =
  IntMap.fromList
  . map (\(Right (v, (rate, valves))) -> (ix v, (rate, map ix valves)))
  . map (parse valveP "")
  . lines

fromIntMap :: IntMap a -> Vector a
fromIntMap m = V.create do
  vec <- MV.new (rangeSize bounds)
  forM_ (IntMap.toList m) \(i, v) ->
    MV.write vec i v
  pure vec

compact :: IntMap (N, [IxValve]) -> Vector (N, [(Int, IxValve)])
compact graph =
  let nonZero = IntMap.filter ((> 0) . fst) graph
      neighbors node = snd $ graph IntMap.! node
      costs from =
        [ (fromJust $ bfs_ (== to) neighbors from, to)
        | to <- IntMap.keys nonZero
        , from /= to
        ]
  in
    fromIntMap . IntMap.fromList
    $ [ (from, (flow, costs from))
      | (from, (flow, _)) <- (ix ('A', 'A'), graph IntMap.! ix ('A', 'A')):IntMap.toList nonZero
      ]

part1 :: IntMap (N, [IxValve]) -> N
part1 vs =
  let compacted = compact vs
      flows = IntMap.filter (> 0) $ IntMap.map fst vs
      potential (_, t, _, p, _) = max 0 (t - 1) * p
      neighbors (!current, !t, !open, !p, !released) = do
        let (_, nexts) = compacted V.! current
        (steps, next) <- nexts
        guard $ steps < t
        guard $ not $ next `IntSet.member` open
        let open' = IntSet.insert next open
            (flow, _) = compacted V.! next
        pure ( next
             , t-steps-1
             , open'
             , p - flow
             , released + (t-steps-1)*flow
             )
      go m =
        \case [] -> m
              c@(_, t, _, p, released):rest
                | t <= 0 || p <= 0 -> go (max released m) rest
                | released + potential c < m -> go m rest
                | otherwise ->
                  let nbhd = neighbors c
                  in
                    if null nbhd
                    then go (max m released) rest
                    else go m (nbhd ++ rest)
  in
    go 0 [(ix ('A', 'A'), 30, IntSet.singleton (ix ('A', 'A')), sum flows, 0)]

part2 :: IntMap (N, [IxValve]) -> N
part2 vs = go 0 [( 26
                 , ix ('A', 'A')
                 , ix ('A', 'A')
                 , 0
                 , IntSet.singleton (ix ('A', 'A'))
                 , sum flows
                 , 0
                 )]
  where compacted = compact vs
        flows = IntMap.filter (> 0) $ IntMap.map fst vs
        potential (t, _, _, _, _, p, _) = max 0 (t - 1) * p
        go m = \case
          [] -> m
          c@(t, node, ttarget, trem, open, p, released):rest
            | t <= 0 || p == 0 -> go (max released m) rest
            | released + potential c < m -> go m rest
            | otherwise ->
              let (_, nexts) = compacted V.! node
                  nbhd = do
                    (steps, next) <- nexts
                    guard $ not $ next `IntSet.member` open
                    guard $ steps < t
                    let (flow, _) = compacted V.! next
                        steps' = steps + 1
                        pressure = max 0 $ flow * (t - steps')
                        p' = p - flow
                        open' = IntSet.insert next open
                    pure $ case compare steps' trem of
                             LT -> (t - steps', next, ttarget, trem - steps', open', p', released + pressure)
                             GT -> (t - trem  , ttarget, next, steps' - trem, open', p', released + pressure)
                             EQ -> (t - trem  , ttarget, next, steps' - trem, open', p', released + pressure)
              in
                if null nbhd
                then go (max released m) rest
                else go m (nbhd ++ rest)

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
