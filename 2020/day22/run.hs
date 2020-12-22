{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Monad.Random
import Control.Monad.State.Strict (State, evalState, get, put)
import Data.Bits (xor)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)

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


type Zobrist = Int


maxDeckSize :: Int
maxDeckSize = 70

zobristMap :: Vector Zobrist
zobristMap =
  let n = maxDeckSize + 1
      v = V.replicateM (n * n) getRandom
  in V.force $ evalRand v (mkStdGen 42)

zobrist :: Int -> Int -> Zobrist
zobrist pos card =
  V.unsafeIndex zobristMap (card * (maxDeckSize + 1) + pos)

stateToHash :: (Deck, Deck) -> Zobrist
stateToHash (p1, p2) =
  foldr xor 0 $ zipWith zobrist [0..] (p1 ++ [0] ++ p2)

type SaveState = Zobrist

toSaveState :: (Deck, Deck) -> SaveState
toSaveState = stateToHash

recursiveCombat :: (Deck, Deck) -> (Winner, Int)
recursiveCombat start =
  evalState (go start) IntSet.empty
  where go :: (Deck, Deck) -> State IntSet (Winner, Int)
        go (p1, []) = pure (P1, score p1)
        go ([], p2) = pure (P2, score p2)
        go setup = do
          seen <- get
          let !ss = toSaveState setup
          if ss `IntSet.member` seen
            then pure (P1, score (fst setup))
            else put (IntSet.insert ss seen) *>
                 go' setup

        go' :: (Deck, Deck) -> State IntSet (Winner, Int)
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
