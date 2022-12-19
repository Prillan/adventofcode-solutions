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

tce :: Show a => String -> a -> a
tce = \_ x -> x
{-# INLINE tce #-}
--tce = AoC.tce

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
  deriving (Eq, Generic, Show)

instance Hashable S

starting = S 24 0 0 0 0 1 0 0 0

-- potential bp@Blueprint {..} S {..} =
--   let (orePerClay, orePerObsidian, orePerGeode) = perMaterial bp

--       current = orePerClay * clay
--                 + orePerObsidian * obsidian
--                 + orePerGeode * geode
--       t' :: Ratio N
--       t' = fromIntegral time
--       perT = oreBots
--              + orePerClay * clayBots
--              + orePerObsidian * obsidianBots
--              + orePerGeode * geodeBots
--   in
--     fromIntegral (geode * orePerGeode)
--     + fromIntegral geodeBots * t'
--     + orePerObsidian * obsidianBots

-- potentialGeode :: Blueprint -> S -> Ratio N
-- potentialGeode bp@Blueprint {..} S {..} =
--   let (orePerClay, orePerObsidian, orePerGeode) = perMaterial bp
--       (geodeOre, geodeObsidian) = geodeBot
--       (obsidianOre, obsidianClay) = obsidianBot

--       geodeRate = 

      
--       geodeBotsAt0 :: Ratio N
--       geodeBotsAt0 =
--         min ((fromIntegral obsidian + t' * fromIntegral obsidianBots) / fromIntegral geodeObsidian)
--             ((fromIntegral ore + t' * fromIntegral oreBots) / fromIntegral geodeOre)

--       obsidianBotsAt0 :: Ratio N
--       obsidianBotsAt0 =
--         min ((fromIntegral clay + t' * fromIntegral clayBots) / fromIntegral obsidianClay)
--             ((fromIntegral ore + t' * fromIntegral oreBots) / fromIntegral obsidianOre)

--       clayBotsAt0 :: Ratio N
--       clayBotsAt0 =
--         (fromIntegral ore + t' * fromIntegral oreBots) / fromIntegral clayBot

--       t' :: Ratio N
--       t' = fromIntegral time
--   in
--     fromIntegral geode
--     + fromIntegral geodeBots * t'
--     + t' * (max 0 $ t' - 1) * geodeBotsAt0 / 2
--     + t' * (max 0 $ t' - 1) * (max 0 $ t' - 2) * obsidianBotsAt0 / 6
--     + t' * (max 0 $ t' - 1) * (max 0 $ t' - 2) * (max 0 $ t' - 3) * clayBotsAt0 / 12

-- potential' bp@Blueprint {..} S {..} =
--   let (orePerClay, orePerObsidian, orePerGeode) = perMaterial bp
--       (geodeOre, geodeObsidian) = geodeBot
--       (obsidianOre, obsidianClay) = obsidianBot

      
--       geodeBotsAt0 :: Ratio N
--       geodeBotsAt0 =
--         min ((fromIntegral obsidian + t' * fromIntegral obsidianBots + t' * fromIntegral cal) / fromIntegral geodeObsidian)
--             ((fromIntegral ore + t' * fromIntegral oreBots) / fromIntegral geodeOre)

--       obsidianBotsAt0 :: Ratio N
--       obsidianBotsAt0 =
--         min ((fromIntegral clay + t' * fromIntegral clayBots) / fromIntegral obsidianClay)
--             ((fromIntegral ore + t' * fromIntegral oreBots) / fromIntegral obsidianOre)

--       clayBotsAt0 :: Ratio N
--       clayBotsAt0 =
--         (fromIntegral ore + t' * fromIntegral oreBots) / fromIntegral clayBot

--       t' :: Ratio N
--       t' = fromIntegral time
--   in
--     (geodeBotsAt0, obsidianBotsAt0, clayBotsAt0)

-- timeToGeode bp@Blueprint {..} s@S{..} =
--   let geode = 

-- diff bp s0 s1 =
--   let (orePerClay, orePerObsidian, orePerGeode) = perMaterial bp

--       current S{..} = orePerClay * clay
--                       + orePerObsidian * obsidian
--                       + orePerGeode * geode
--   in
--     potential bp s1 - potential bp s0

-- diffGeode bp s0 s1 =
--   potentialGeode bp s1 - potentialGeode bp s0

-- perMaterial Blueprint {..} =
--   let (geodeOre, geodeObsidian) = geodeBot
--       (obsidianOre, obsidianClay) = obsidianBot

--       clay     = clayBot
--       obsidian = obsidianOre + obsidianClay * clay
--       geode    = geodeOre + geodeObsidian * obsidian
--   in
--     (clay, obsidian, geode)

-- perMaterial' Blueprint {..} =
--   let (geodeOre, geodeObsidian) = geodeBot
--       (obsidianOre, obsidianClay) = obsidianBot

--       clay     = clayBot
--       obsidian = obsidianOre + obsidianClay * clay
--       geode    = geodeOre + geodeObsidian * obsidian
--   in
--     (clay, obsidian, geode)

--optimize :: Blueprint -> N -> Maybe ((N, S), Ratio N)
optimize bp@Blueprint{..} t0 =
  let neighbors !s = do
        next <- moves (tce "exploring" s)
        let !t = timeTo bp (tce " next" next) s
        case next of
          End -> pure $ tick t s
          _   -> do
            guard $ (tce "  timeto" t) < time s
            let !s' = tce "  ticked" $ tick t s
            pure $ tce "  constructed" $ constructTick bp s' next
      go !s
        | time s <  0 = error $ "FAIL: " ++ show s
        | time s == 0 = [s]
        | time s == 1 = [s]
        | otherwise =
          concatMap go (neighbors s)
  in
    go starting { time = t0 }
    --    go starting { time = t0 }
--    astar stop neighbors (negate . v3 . potential' bp) starting { time = t0 }

test bp = scanl f starting cases
  where f s case_@(idx, action, target_s) =
          let Just next = find (== action) (moves s)
              t = timeTo bp next s
              s' = tick t s
              c = constructTick bp s' next
          in 
            if c == target_s
            then c
            else error $ unlines [ "not equal!"
                                 , "case: " <> show case_
                                 , "before  : " <> show s
                                 , "got     : " <> show c
                                 , "expected: " <> show target_s
                                 ]
          

cases =
  [ ( 3
    , Clay
    , starting
      { time = 24 - 3
      , ore = 1
      , oreBots = 1
      , clayBots = 1
      }
    )

  , ( 5
    , Clay
    , starting
      { time = 24 - 5
      , oreBots = 1
      , ore = 1
      , clayBots = 2
      , clay = 2
      }
    )

  , ( 7
    , Clay
    , starting
      { time = 24 - 7
      , oreBots = 1
      , ore = 1
      , clayBots = 3
      , clay = 6
      }
    )

  , ( 11
    , Obsidian
    , starting
      { time = 24 - 11
      , oreBots = 1
      , ore = 2
      , clayBots = 3
      , clay = 4
      , obsidianBots = 1
      }
    )
  
  , ( 12
    , Clay
    , starting
      { time = 24 - 12
      , oreBots = 1
      , ore = 1
      , clayBots = 4
      , clay = 7
      , obsidianBots = 1
      , obsidian = 1
      }
    )
-- (Ore, 1, 1, 1) --bots, from to
-- (Clay, 3, 3, 7) -- bots, from to
-- (Obsidian, 1, 1, 1)
-- (Clay, 4)

-- 13
-- (Ore, 1, 1, 2) --bots, from to
-- (Clay, 4, 4, 1) -- bots, from to
-- (Obsidian, 1, 1, 2)

-- 14
-- (Ore, 1, 1, 3) --bots, from to
-- (Clay, 4, 4, 1) -- bots, from to
-- (Obsidian, 1, 1, 3)

-- 15
-- Obsidian
-- (Ore, 1, 1, 1) --bots, from to
-- (Clay, 4, 4, 5) -- bots, from to
-- (Obsidian, 1, 1, 4)
-- (Obsidian, 2) -- new

-- 16
-- (Ore, 1, 1, 2) --bots, from to
-- (Clay, 4, 4, 9) -- bots, from to
-- (Obsidian, 2, 2, 6)

-- 17
-- (Ore, 1, 1, 3) --bots, from to
-- (Clay, 4, 4, 1) -- bots, from to
-- (Obsidian, 2, 2, 8)

-- 18
-- Geode
-- (Ore, 1, 1, 2) --bots, from to
-- (Clay, 4, 4, 1) -- bots, from to
-- (Obsidian, 2, 2, 3)
-- (Geode, 1)

-- 19
-- (Ore, 1, 1, 3) --bots, from to
-- (Clay, 4, 4, 2) -- bots, from to
-- (Obsidian, 2, 2, 5)
-- (Geode, 1, 1, 1)

-- 20
-- (Ore, 1, 1, 4) --bots, from to
-- (Clay, 4, 4, 2) -- bots, from to
-- (Obsidian, 2, 2, 7)
-- (Geode, 1, 1, 2)

-- 21
-- Geode
-- (Ore, 1, 1, 3) --bots, from to
-- (Clay, 4, 4, 2) -- bots, from to
-- (Obsidian, 2, 2, 2)
-- (Geode, 1, 1, 3)
-- (Geode, 2)

-- 22
-- (Ore, 1, 1, 4) --bots, from to
-- (Clay, 4, 4, 3) -- bots, from to
-- (Obsidian, 2, 2, 4)
-- (Geode, 2, 2, 5)

-- 23
-- (Ore, 1, 1, 5) --bots, from to
-- (Clay, 4, 4, 3) -- bots, from to
-- (Obsidian, 2, 2, 6)
-- (Geode, 2, 2, 7)

-- 24
-- (Ore, 1, 1, 6) --bots, from to
-- (Clay, 4, 4, 4) -- bots, from to
-- (Obsidian, 2, 2, 8)
-- (Geode, 2, 2, 9)

  ]

timeTo Blueprint {..} target s@S{..} =
  let (geodeOre, geodeObsidian) = geodeBot
      (obsidianOre, obsidianClay) = obsidianBot
  in
    case target of
      Geode    -> max 0 $ max (ceiling $ (geodeObsidian - obsidian) % obsidianBots)
                              (ceiling $ (geodeOre - ore) % oreBots)
      Obsidian -> max 0 $ max (ceiling $ (obsidianClay - clay) % clayBots)
                              (ceiling $ (obsidianOre - ore) % oreBots)
      Clay     -> max 0 $ ceiling $ (clayBot - ore) % oreBots
      Ore      -> max 0 $ ceiling $ (oreBot - ore) % oreBots
      End      -> time
      
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
  concat $
  [ if obsidianBots >= 1 && time >= 2 then [Geode]    else []
  , if clayBots     >= 1 && time >= 3 then [Obsidian] else []
  , if                      time >= 4 then [Clay]     else []
  , if                      time >= 5 then [Ore]      else []
  , if geodeBots    >= 1              then [End]      else []
  ]

data Move = Geode
          | Obsidian
          | Clay
          | Ore
          | End
  deriving (Show, Eq)

part1 =
  map (\bp -> (bpId bp, maximum $ map geode (optimize bp 24)))
  
part2 = const ()

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
