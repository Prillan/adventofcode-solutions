{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

-- TODO: Clean up

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
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

type N = Int

cards = reverse "AKQJT98765432"
strength c = maybe (-1) id $ findIndex (== c) cards

jcards = reverse "AKQT98765432"
jstrength c = maybe (-1) id $ findIndex (== c) jcards

type Card = Char
data Types =
  Five | Four | FullHouse | Three | TwoPair | OnePair | High
  deriving (Eq, Show, Enum, Bounded, Ord)

newtype Hand = Hand { unHand :: [Card] }
  deriving (Eq, Show)

instance Ord Hand where
  compare h1 h2 =
    comparing (Down . type_) h1 h2 <> comparing (map strength . unHand) h1 h2

data JHand = JHand { original :: Hand
                   , best     :: Hand
                   }
  deriving (Eq, Show)

instance Ord JHand where
  compare h1 h2 =
    comparing (Down . type_ . best) h1 h2 <> comparing (map jstrength . unHand . original) h1 h2

type Shape = [N]

shape :: Hand -> [Int]
shape =
  map snd
  . sortOn (negate . snd)
  . Map.toList
  . counter
  . unHand

type_ :: Hand -> Types
type_ h =
  case shape h of
    5:_   -> Five
    4:_   -> Four
    3:2:_ -> FullHouse
    3:_   -> Three
    2:2:_ -> TwoPair
    2:_   -> OnePair
    1:_   -> High

jhand :: Hand -> JHand
jhand (Hand original) =
  JHand { original = Hand original
        , best = maximum . map Hand $ traverse joker original
        }

joker :: Card -> [Card]
joker = \case 'J' -> jcards
              c   -> [c]

parse input =
  let [hand, bid] = words input
  in (hand, read @N bid)

parseAll = map parse  . lines

score =
  sum
  . zipWith (\r (_, bid) -> r * bid) [1..]

part1 =
  score
  . sortBy (comparing (Hand . fst))

part2 =
  score
  . sortBy (comparing fst)
  . map (first (jhand . Hand))

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
