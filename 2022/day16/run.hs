{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Search (bfs_)

import Control.Monad (guard, replicateM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Valve = (Char, Char)
type N = Int

type Parser a = Parsec Void String a

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

parseAll :: String -> HashMap Valve (N, [Valve])
parseAll =
  HashMap.fromList
  . map (\(Right x) -> x)
  . map (parse valveP "")
  . lines

compact :: HashMap Valve (N, [Valve]) -> HashMap Valve (N, [(Int, Valve)])
compact graph =
  let nonZero = HashMap.filter ((> 0) . fst) graph
      neighbors node = snd $ graph HashMap.! node
      costs from =
        [ (fromJust $ bfs_ (== to) neighbors from, to)
        | to <- HashMap.keys nonZero
        , from /= to
        ]
  in
    HashMap.fromList
    $ [ (from, (flow, costs from))
      | (from, (flow, _)) <- (('A', 'A'), graph HashMap.! ('A', 'A')):HashMap.toList nonZero
      ]

part1 :: HashMap Valve (N, [Valve]) -> N
part1 vs =
  let compacted = compact vs
      flows = HashMap.filter (> 0) $ HashMap.map fst vs
      potential (_, t, _, p, _) = max 0 (t - 1) * p
      neighbors (!current, !t, !open, !p, !released) = do
        let (!flow, nexts) = compacted HashMap.! current
        toggle <- if flow == 0 || current `HashSet.member` open
                  then [False]
                  else [True, False]
        case toggle of
          True  ->
            let open' = HashSet.insert current open
            in pure $ (current, t-1, open', p - flow, released + (t-1)*flow)
          False -> do
            (steps, next) <- nexts
            pure $ (next, t-steps, open, p, released)
      go m =
        \case [] -> m
              c@(_, t, _, p, released):rest
                | t <= 0 || p <= 0 -> go (max released m) rest
                | released + potential c < m -> go m rest
                | otherwise ->
                    go m (neighbors c ++ rest)
  in
    go 0 [(('A', 'A'), 30, HashSet.empty, sum flows, 0)]

part2 :: HashMap Valve (N, [Valve]) -> N
part2 vs = go 0 [(26, ('A', 'A'), ('A', 'A'), 0, HashSet.singleton ('A', 'A'), sum flows, 0)]
  where compacted = compact vs
        flows = HashMap.filter (> 0) $ HashMap.map fst vs
        potential (t, _, _, _, _, p, _) = max 0 (t - 1) * p
        go m = \case
          [] -> m
          c@(t, node, ttarget, trem, open, p, released):rest
            | t <= 0 || p == 0 -> go (max released m) rest
            | released + potential c < m -> go m rest
            | otherwise ->
              let (_, nexts) = compacted HashMap.! node
                  nbhd = do
                    (steps, next) <- nexts
                    guard $ not $ next `HashSet.member` open
                    guard $ steps <= t
                    pure (steps, next)
              in
                case nbhd of
                  [] -> go (max released m) rest
                  xs ->
                    let new = do
                          (steps, next) <- xs
                          let flow = flows HashMap.! next
                              steps' = steps + 1
                              pressure = max 0 $ flow * (t - steps')
                              p' = p - flow
                              open' = HashSet.insert next open
                          pure $ case compare steps' trem of
                            LT -> (t - steps', next, ttarget, trem - steps', open', p', released + pressure)
                            GT -> (t - trem  , ttarget, next, steps' - trem, open', p', released + pressure)
                            EQ -> (t - trem  , ttarget, next, steps' - trem, open', p', released + pressure)
                    in
                      go m (new ++ rest)

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
