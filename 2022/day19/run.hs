{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
import AoC hiding (tce)
import qualified AoC
import AoC.Grid
import AoC.Search

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
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import GHC.Generics (Generic)
import Data.Hashable

import Data.Ratio (Ratio, (%))

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String
type N = Int

data Blueprint = Blueprint { bpId        :: Int
                           , oreBot      :: N
                           , clayBot     :: N
                           , obsidianBot :: (N, N)
                           , geodeBot    :: (N, N)
                           }
  deriving Show

numP :: (Num a, Read a) => Parser a
numP = read <$> many digitChar

blueprintP =
  Blueprint <$> (string "Blueprint " *> numP <* string ": ")
            <*> (string "Each ore robot costs " *> numP <* string " ore. ")
            <*> (string "Each clay robot costs " *> numP <* string " ore. ")
            <*> ((,) <$> (string "Each obsidian robot costs " *> numP) <*> (string " ore and " *> numP <* string " clay. "))
            <*> ((,) <$> (string "Each geode robot costs " *> numP) <*> (string " ore and " *> numP <* string " obsidian."))

parseAll = map (\case (Right x) -> x) . map (parse blueprintP "")  . lines

data S = S { time :: !Int
           , ore :: !Int
           , clay :: !Int
           , obsidian :: !Int
           , geode :: !Int
           , oreBots :: !Int
           , clayBots :: !Int
           , obsidianBots :: !Int
           , geodeBots :: !Int
           }
  deriving (Eq, Ord, Generic, Show)

instance Hashable S

starting = S 24 0 0 0 0 1 0 0 0

optimize :: Blueprint -> N -> N
optimize bp@Blueprint{..} t0 =
  let neighbors !s = do
        next <- moves s
        let !t = timeTo bp next s
        case next of
          End -> pure $ tick t s
          _   -> do
            guard $ t < time s
            let !s' = tick t s
            pure $ constructTick bp s' next
      go m [] = m
      go !m (!s:fs)
        | time s <  0 = error $ "FAIL: " ++ show s
        | time s == 0 = go (max (geode s) m) fs
        | time s == 1 = let s' = tick 1 s
                        in go (max (geode s') m) fs
        | theoreticalMax s < m = go m fs
        | otherwise = go m (neighbors s ++ fs)
  in
    go 0 [starting { time = t0 }]

theoreticalMax S{..} =
  geode
  + geodeBots * time
  + time * (time - 1) `div` 2

timeTo Blueprint {..} target s@S{..} =
  let (geodeOre, geodeObsidian) = geodeBot
      (obsidianOre, obsidianClay) = obsidianBot
  in
    case target of
      Geode    -> max 0 $ max (timeTo' obsidian geodeObsidian obsidianBots)
                              (timeTo' ore geodeOre oreBots)
      Obsidian -> max 0 $ max (timeTo' clay obsidianClay clayBots)
                              (timeTo' ore obsidianOre oreBots)
      Clay     -> max 0 $ timeTo' ore clayBot oreBots
      Ore      -> max 0 $ timeTo' ore oreBot oreBots
      End      -> time

timeTo' :: N -> N -> N -> N
timeTo' current target bots =
  case (target - current) `divMod` bots of
    (x, 0) -> x
    (x, _) -> x + 1
{-# INLINE timeTo' #-}

constructTick Blueprint {..} s@S {..} =
  let (geodeOre, geodeObsidian) = geodeBot
      (obsidianOre, obsidianClay) = obsidianBot
  in
    \case Geode -> s { time      = time - 1
                     , geodeBots = geodeBots + 1
                     , geode     = geode + geodeBots
                     , obsidian  = obsidian  + obsidianBots - geodeObsidian
                     , clay      = clay + clayBots
                     , ore       = ore + oreBots - geodeOre }
          Obsidian -> s { time         = time - 1
                        , obsidianBots = obsidianBots + 1
                        , geode        = geode + geodeBots
                        , obsidian     = obsidian + obsidianBots
                        , clay         = clay + clayBots - obsidianClay
                        , ore          = ore + oreBots - obsidianOre
                        }
          Clay -> s { time       = time - 1
                    , clayBots   = clayBots + 1
                    , geode      = geode + geodeBots
                    , obsidian   = obsidian + obsidianBots
                    , clay       = clay + clayBots
                    , ore        = ore + oreBots - clayBot
                    }
          Ore -> s { time     = time - 1
                   , oreBots  = oreBots + 1
                   , geode    = geode + geodeBots
                   , obsidian = obsidian + obsidianBots
                   , clay     = clay + clayBots
                   , ore      = ore + oreBots - oreBot
                   }
          End -> s

tick n s@S {..} =
  s { time = time - n
    , ore = ore + n * oreBots
    , clay = clay + n * clayBots
    , obsidian = obsidian + n * obsidianBots
    , geode = geode + n * geodeBots
    }

moves s@S {..} =
  let m = concat $
        [ if obsidianBots >= 1 && time >= 2 then [Geode]    else []
        , if clayBots     >= 1 && time >= 2 then [Obsidian] else []
        , if                      time >= 3 then [Clay]     else []
        , if                      time >= 4 then [Ore]      else []
        ]
  in
    if null m && geodeBots >= 1
    then [End]
    else m

data Move = Geode
          | Obsidian
          | Clay
          | Ore
          | End
  deriving (Show, Eq)

part1 =
   sum
  . map (\bp -> bpId bp * (optimize bp 24))

part2 =
  product
  . map (`optimize` 32)
  . take 3

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
