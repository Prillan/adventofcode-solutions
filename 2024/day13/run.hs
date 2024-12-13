{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Parse

import Data.Ratio ((%), denominator, numerator)
import Data.Void (Void)
import Data.List.Split (splitOn)

import qualified Data.Matrix as M

import Text.Megaparsec
import Text.Megaparsec.Char

type N = Integer
type InputRow = ((N, N), (N, N), (N, N))
type Input = [InputRow]

type Parser = Parsec Void String

buttonP :: Parser (N, N)
buttonP = do
  _ <- string "Button "
  _ <- anySingle
  _ <- string ": X+"
  x <- numP @N
  _ <- string ", Y+"
  y <- numP @N
  pure (x, y)

prizeP :: Parser (N, N)
prizeP = do
  _ <- string "Prize: X="
  x <- numP @N
  _ <- string ", Y="
  y <- numP @N
  pure (x, y)


parse :: [String] -> InputRow
parse = \[a, b, p] ->
  fromRight $ (,,) <$> runParser buttonP "a" a
                   <*> runParser buttonP "b" b
                   <*> runParser prizeP "p" p

fromRight :: Show a => Either a b -> b
fromRight = \case Right x -> x
                  Left e -> error $ show e

parseAll :: String -> Input
parseAll = map Main.parse . splitOn [""] . lines

solve :: Bool -> InputRow -> Maybe (N, N)
solve limit ((x11, x12), (x21, x22), (p1, p2)) =
  let f = (% 1)
      m = M.fromLists [[f x11, f x21], [f x12, f x22]]
      p = M.fromLists [[f p1], [f p2]]
  in case M.inverse m of
    Left _ -> Nothing
    Right m' | [a, b] <- M.toList $ m' `M.multStd` p
             , denominator a == 1
             , denominator b == 1
             , not limit || a <= 100
             , not limit || b <= 100
               -> Just (numerator a, numerator b)
    _ -> Nothing

cost :: Bool -> InputRow -> N
cost limit =
  maybe 0 (\(a, b) -> 3 * a + b) . solve limit

part1 :: Input -> N
part1 = sum . map (cost True)

part2 :: Input -> N
part2 = sum . map (cost False) . map stupidIncrement
  where stupidIncrement (a, b, (px, py)) = (a, b, (px + 10000000000000, py + 10000000000000))

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
