{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
import AoC

import Control.Monad (guard)
import Data.Hashable (Hashable)
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Char

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

blueprintP :: Parser Blueprint
blueprintP =
  Blueprint <$> (string "Blueprint " *> numP <* string ": ")
            <*> (string "Each ore robot costs " *> numP <* string " ore. ")
            <*> (string "Each clay robot costs " *> numP <* string " ore. ")
            <*> ((,) <$> (string "Each obsidian robot costs " *> numP) <*> (string " ore and " *> numP <* string " clay. "))
            <*> ((,) <$> (string "Each geode robot costs " *> numP) <*> (string " ore and " *> numP <* string " obsidian."))

parseAll :: String -> [Blueprint]
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

starting :: S
starting = S 24 0 0 0 0 1 0 0 0

-- TODO: Extract BFS trying to find the maximum with heurestic for
-- early exit, to AoC.Search.

optimize :: Blueprint -> N -> N
optimize bp t0 =
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

-- TODO: Improve
theoreticalMax :: S -> Int
theoreticalMax S{..} =
  geode -- current geode
  + geodeBots * time -- gained by current geode bots
  + time * (time - 1) `div` 2 -- gained by new geode bots if we add
                              -- one per tick. It's an upper bound,
                              -- since we might not have enough bots
                              -- of the other kinds to produce one
                              -- geode bot per tick. A better bound
                              -- would take this into account.

timeTo :: Blueprint -> Move -> S -> N
timeTo Blueprint {..} target S {..} =
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


constructTick :: Blueprint -> S -> Move -> S
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

tick :: Int -> S -> S
tick n s@S {..} =
  s { time = time - n
    , ore = ore + n * oreBots
    , clay = clay + n * clayBots
    , obsidian = obsidian + n * obsidianBots
    , geode = geode + n * geodeBots
    }

moves :: S -> [Move]
moves S {..} =
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

part1 :: [Blueprint] -> Int
part1 =
   sum
  . map (\bp -> bpId bp * optimize bp 24)

part2 :: [Blueprint] -> Int
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
