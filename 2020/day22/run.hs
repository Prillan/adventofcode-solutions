{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Monad.State (State, evalState, get, put)

type Deck = [Int]

parse :: String -> Deck
parse =
  map read
  . drop 1
  . filter (not . null)
  . lines

parseAll :: String -> (Deck, Deck)
parseAll s =
  let [p1, p2] =
        map parse
        . filter (not . null)
        . splitOn "\n\n"
        $ s
  in (p1, p2)

combat :: (Deck, Deck) -> Int
combat = go
  where go (p1, []) = score p1
        go ([], p2) = score p2
        go (x:p1, y:p2)
          | x > y     = go (p1 ++ [x, y], p2)
          | otherwise = go (p1, p2 ++ [y, x])

score :: Deck -> Int
score = sum . zipWith (*) [1..] . map fromIntegral . reverse

data Winner = P1 | P2
  deriving (Show, Eq)

recursiveCombat :: (Deck, Deck) -> (Winner, Int)
recursiveCombat = flip evalState Set.empty . go
  where go :: (Deck, Deck) -> State (Set (Deck, Deck)) (Winner, Int)
        go (p1, []) = pure (P1, score p1)
        go ([], p2) = pure (P2, score p2)
        go setup = do
          !seen <- get
          if setup `Set.member` seen
            then pure (P1, score (fst setup))
            else put (Set.insert setup seen) *> go' setup

        go' :: (Deck, Deck) -> State (Set (Deck, Deck)) (Winner, Int)
        go' (x:p1, y:p2)
          | x <= length p1 && y <= length p2 =
            case recursiveCombat (take x p1, take y p2) of
              (P1, _) -> go (p1 ++ [x, y], p2)
              (P2, _) -> go (p1, p2 ++ [y, x])
          | x > y     = go (p1 ++ [x, y], p2)
          | otherwise = go (p1, p2 ++ [y, x])

part1 = combat
part2 = snd . recursiveCombat

main = main' "input.txt"
exampleMain = main' "example.txt"
example2Main = main' "example2.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
