{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Bifunctor
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

-- TODO: Improve later

type Parser = Parsec Void String
type N = Int


-- Two main representations, one is better suited for "explode" and
-- "split".
type ListNum = [(Int, N)]

-- ... and one is easier to use for "magnitude" and printing
data TreeNum = Regular Int
            | Pair TreeNum TreeNum
  deriving (Eq, Show)


num :: Parser ListNum
num = val 0

val :: Int -> Parser ListNum
val n = pair n <|> lit n

lit :: Int -> Parser ListNum
lit n = do
  v <- read <$> many digitChar
  pure [(n, v)]

pair :: Int -> Parser ListNum
pair n = between (char '[') (char ']') do
  l <- val (n + 1)
  char ','
  r <- val (n + 1)
  pure $ l ++ r

add :: ListNum -> ListNum -> ListNum
add l r = reduce . map (first (+1)) $ l ++ r

reduce :: ListNum -> ListNum
reduce = fixpoint (split . fixpoint explode)
  where explode = \case
          [] -> []
          (dl, vl):(d1, v1):(d2, v2):[]
            | d1 == 5 && d2 == 5 ->
              (dl, vl + v1):(4, 0):[]
          (d1, v1):(d2, v2):(dr, vr):rest
            | d1 == 5 && d2 == 5 ->
              (4, 0):(dr, vr + v2):rest
          (dl, vl):(d1, v1):(d2, v2):(dr, vr):rest
            | d1 == 5 && d2 == 5 ->
              (dl, vl + v1):(4, 0):(dr, v2 + vr):rest
          x:rest -> x:explode rest
        split = \case
          [] -> []
          (d, v):rest
            | v < 10    -> (d, v):split rest
            | otherwise ->
                let (q, r) = v `divMod` 2
                in (d + 1, q):(d + 1, q + r):rest

pp :: TreeNum -> String
pp = go
  where go = \case
          Regular v -> show v
          Pair v1 v2 -> "[" ++ go v1 ++ "," ++ go v2 ++ "]"

tree :: ListNum -> TreeNum
tree = fst . val 0
  where pair n xs =
          let (l, xs') = val n xs
              (r, xs'') = val n xs'
          in (Pair l r, xs'')

        val n = \case
          xs@((d, v):rest)
            | d > n  -> pair (n + 1) xs
            | d == n -> (Regular v, rest)

magnitude :: TreeNum -> Int
magnitude = go
  where go = \case
          Regular v  -> v
          Pair v1 v2 -> 3 * go v1 + 2 * go v2

parseAll =
  map (\case Right v -> v)
  . map (parse num "")
  . lines

part1 = magnitude . tree . foldl1 add
part2 input =
  maximum
  . map (magnitude . tree)
  $ [ x1 `add` x2 | x1 <- input
                  , x2 <- input
                  , x1 /= x2 ]

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
