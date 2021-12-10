{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Applicative
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord

data Error = Incomplete String
           | Corrupt Char Char
           | InputLeft String
  deriving (Show, Eq)

open  = "(<[{"
close = \case '(' -> ')'
              '<' -> '>'
              '[' -> ']'
              '{' -> '}'

parser :: String -> Either Error ()
parser = go []
  where go st xs = case (st, xs) of
                     (_, x:xs')
                       | x `elem` open -> go (x:st) xs' -- new open, push to stack
                     ([], _:_) -> Left (InputLeft xs)   -- close, but no stack!
                     (s:st', x:xs')
                       | close s == x -> go st' xs' -- close matches stack, pop
                       | otherwise    -> Left (Corrupt (close s) x) -- corrupt!
                     ([], []) -> Right () -- done!
                     (st, []) -> Left (Incomplete (map close st)) -- parts left to consume!

charScore1 = \case ')' -> 3
                   ']' -> 57
                   '}' -> 1197
                   '>' -> 25137

score1 = \case Left (Corrupt _ c) -> charScore1 c
               _                  -> 0

charScore2 = \case ')' -> 1
                   ']' -> 2
                   '}' -> 3
                   '>' -> 4

score2 =
  \case Left (Incomplete st) ->
          foldl' (\s c -> s * 5 + c) 0 (map charScore2 st)
        _ -> 0

part1 =
  sum
  . map score1
  . map parser

part2 =
  round
  . median
  . filter (> 0)
  . map score2
  . map parser

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- lines <$> readFile file
   print (part1 input)
   print (part2 input)
