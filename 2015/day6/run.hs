{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
import AoC

import Text.Parsec

import Data.Functor (($>))
import Data.Foldable
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V

type Position = V2 Int

data Command = Turn LightState | Toggle
  deriving Show
data Instruction = I Position Position Command
  deriving Show

data LightState = On | Off
  deriving (Eq, Show)

number = read <$> many1 digit

position = v2 <$> ((,) <$> number <*> (char ',' *> number))
command = try (string "turn on" $> Turn On)
  <|> try (string "turn off" $> Turn Off)
  <|> string "toggle" $> Toggle

instruction = do
  cmd <- command
  _ <- char ' '
  start <- position
  _ <- string " through "
  end <- position
  pure $ I start end cmd

toUpdate :: Position -> Position -> [Position]
toUpdate (V2 (sx, sy)) (V2 (ex, ey)) = sequence $ v2 ([sx..ex], [sy..ey])

cmdValue :: Command -> Int
cmdValue = \case Turn On -> 1
                 Turn Off -> -1
                 Toggle -> 2

execCmd :: Command -> Int -> Int
execCmd = \case Turn On  -> const 1
                Turn Off -> const 0
                Toggle   -> \case 0 -> 1
                                  1 -> 0

parseAll :: String -> [Instruction]
parseAll =
  map (\(Right v) -> v)
  . map (parse instruction "")
  . lines

vecIndex :: Position -> Int
vecIndex (V2 (x, y)) = x + y * 1000

process :: (Command -> Int -> Int) -> [Instruction] -> V.Vector Int
process f input = V.create do
  vec <- M.new (1000 * 1000)
  M.set vec 0
  forM_ input \(I start end cmd) ->
    forM_ (toUpdate start end) \p ->
      M.unsafeModify vec (f cmd) (vecIndex p)
  pure vec

part1 :: [Instruction] -> Int
part1 = V.sum . process execCmd

part2 :: [Instruction] -> Int
part2 = V.sum . process (\cmd v -> max (v + cmdValue cmd) 0)

main :: IO ()
main = do
  input <- parseAll <$> readFile "input.txt"
  print (part1 input)
  print (part2 input)
