{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid
import AoC.Search
import Debug.Trace (trace)

import Control.Monad (guard)
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


import Data.Bifunctor (second)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQueue


type N = Int

valves :: HashMap String (Int, [String])
valves = HashMap.fromList
  . map (\(v, flow, asdf) -> (v, (flow, splitOn ", " asdf)))
  $ [ ("TM", 0, "KF, AA")
  , ("LG", 8, "DD, UA")
  , ("IZ", 20, "LY, XC")
  , ("XF", 0, "PB, QD")
  , ("FE", 0, "ZW, KF")
  , ("ZP", 0, "MT, AI")
  , ("CL", 0, "JN, AI")
  , ("UA", 0, "VW, LG")
  , ("VP", 0, "MB, GU")
  , ("KY", 0, "BZ, CJ")
  , ("AI", 11, "TL, GG, CL, ZP, MM")
  , ("GD", 0, "KB, QK")
  , ("GU", 14, "ZK, VP")
  , ("RO", 0, "KS, TJ")
  , ("VW", 0, "UA, KS")
  , ("YE", 24, "DP")
  , ("AA", 0, "TL, ZU, TM, RL, BZ")
  , ("RL", 0, "AA, NU")
  , ("RG", 0, "CJ, KS")
  , ("ZW", 0, "TJ, FE")
  , ("OY", 0, "KS, AO")
  , ("CE", 0, "QK, IQ")
  , ("JN", 0, "EK, CL")
  , ("OF", 0, "KS, ZK")
  , ("LY", 0, "IZ, EJ")
  , ("DD", 0, "KF, LG")
  , ("QK", 15, "CE, EJ, UK, GD")
  , ("XC", 0, "RA, IZ")
  , ("EK", 22, "JN")
  , ("JM", 0, "VF, KF")
  , ("UK", 0, "PB, QK")
  , ("ZK", 0, "GU, OF")
  , ("EJ", 0, "LY, QK")
  , ("CJ", 10, "WS, IQ, RG, KY")
  , ("MB", 18, "VP")
  , ("TL", 0, "AA, AI")
  , ("KS", 13, "OF, OY, RO, RG, VW")
  , ("QD", 0, "XF, TJ")
  , ("CU", 19, "AO, DP")
  , ("PB", 5, "ZU, GG, XF, UK, VF")
  , ("KF", 7, "DD, JM, ZH, FE, TM")
  , ("TJ", 3, "QD, ZW, NU, RO, MT")
  , ("ZH", 0, "KF, WS")
  , ("BZ", 0, "KY, AA")
  , ("NU", 0, "RL, TJ")
  , ("KB", 21, "RA, GD, JW")
  , ("WS", 0, "ZH, CJ")
  , ("ZU", 0, "PB, AA")
  , ("MT", 0, "ZP, TJ")
  , ("JW", 0, "MM, KB")
  , ("DP", 0, "CU, YE")
  , ("AO", 0, "OY, CU")
  , ("RA", 0, "KB, XC")
  , ("VF", 0, "PB, JM")
  , ("IQ", 0, "CE, CJ")
  , ("GG", 0, "AI, PB")
  , ("MM", 0, "AI, JW")
  ]

examples :: HashMap String (Int, [String])
examples = HashMap.fromList
  . map (\(v, flow, asdf) -> (v, (flow, splitOn ", " asdf)))
  $
  [ ("AA", 0, "DD, II, BB")
  , ("BB", 13, "CC, AA")
  , ("CC", 2, "DD, BB")
  , ("DD", 20, "CC, AA, EE")
  , ("EE", 3, "FF, DD")
  , ("FF", 0, "EE, GG")
  , ("GG", 0, "FF, HH")
  , ("HH", 22, "GG")
  , ("II", 0, "AA, JJ")
  , ("JJ", 21, "II")
  ]
  

parse = id

parseAll = map parse  . lines


compact :: HashMap String (N, [String]) -> HashMap String (N, [(Int, String)])
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
      | (from, (flow, _)) <- ("AA", graph HashMap.! "AA"):HashMap.toList nonZero
      ]
--part1 :: HashMap String (N, [String]) -> Maybe Int
part1 vs =
  -- node = (valve, t, open)
  let compacted = compact vs
      n = length . filter (> 0) . map fst $ HashMap.elems vs
      flows = HashMap.filter (> 0) $ HashMap.map fst vs
      stop (_, t, _, p) = t < 1 || p == 0
      potential (_, t, _, p) = negate $ max 0 (t - 1) * p
      neighbors (current, t, open, p) = do
        let (!flow, nexts) = compacted HashMap.! current
        toggle <- if flow == 0 || current `HashSet.member` open
                  then [False]
                  else [True, False]
        case toggle of
          True  ->
            let open' = HashSet.insert current open
            in pure $ ((current, t-1, open', p - flow), -(t-1)*flow)
          False -> do
            (steps, next) <- nexts
            pure $ ((next, t-steps, open, p), 0)
  in astar stop neighbors potential ("AA", 30, HashSet.empty, sum flows)

part2 vs = maximum $ go 26 "AA" "AA" 0 ["AA"] 0
  where compacted = compact vs
        flows = HashMap.filter (> 0) $ HashMap.map fst vs
        go t node ttarget trem open released
          | t <= 0    = [released]
          | otherwise =
            let (_, nexts) = compacted HashMap.! node
                potential = do
                  (steps, next) <- nexts
                  guard $ not $ next `elem` open
                  guard $ steps <= t
                  pure (steps, next)
            in
              case potential of
                [] -> [released]
                xs -> do
                  (steps, next) <- xs
                  let flow = flows HashMap.! next
                      steps' = steps + 1
                      pressure = max 0 $ flow * (t - steps')
                  case compare steps' trem of
                    LT -> go (t - steps') next ttarget (trem - steps') (next:open) (released + pressure)
                    GT -> go (t - trem)   ttarget next (steps' - trem) (next:open) (released + pressure)
                    EQ -> go (t - trem)   ttarget next (steps' - trem) (next:open) (released + pressure)

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
--   input <- parseAll <$> readFile file
   print (part1 valves)
   print (part2 valves)

   print (part1 examples)

   print (compact examples)
   print (part2 examples)
