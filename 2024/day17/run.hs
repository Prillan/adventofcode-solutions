{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC

import Control.Monad (guard)
import Data.Bits
import Data.List.Split (splitOn)

type N = Int

type Input = ([N], [N])

data S = S { regA :: N
           , regB :: N
           , regC :: N
           , pc   :: N
           }
  deriving Show

initial :: N -> N -> N -> S
initial a b c = S { regA = a
                  , regB = b
                  , regC = c
                  , pc   = 0
                  }

parseProgram :: [N] -> S -> (S, Maybe N)
parseProgram = parse
  where combo = \case 4 -> regA
                      5 -> regB
                      6 -> regC
                      7 -> error "invalid 7"
                      i -> const i
        parse = \case [0, op] -> \s -> (s {regA = regA s `shiftR` combo op s}, Nothing)
                      [1, op] -> \s -> (s {regB = regB s `xor` op}           , Nothing)
                      [2, op] -> \s -> (s {regB = combo op s `mod` 8}        , Nothing)
                      [3, op] -> \case s | regA s == 0 -> (s                 , Nothing)
                                         | otherwise   -> (s { pc = op - 2 } , Nothing)
                      [4,  _] -> \s -> (s {regB = regB s `xor` regC s}       , Nothing)
                      [5, op] -> \s -> (s                                    , Just (combo op s `mod` 8))
                      [6, op] -> \s -> (s {regB = regA s `shiftR` combo op s}, Nothing)
                      [7, op] -> \s -> (s {regC = regA s `shiftR` combo op s}, Nothing)

runProgram :: [N] -> N -> N -> N -> [N]
runProgram prog a b c = go (initial a b c)
  where n = length prog
        valid s = 0 <= pc s && pc s < n
        go s | valid s = case step s of
                           (s', Just r) -> r:go s'
                           (s', _)      -> go s'
             | otherwise = []
        step s = let ip = pc s
                     (!new, r) = parseProgram (take 2 (drop ip prog)) s
                 in (new { pc = pc new + 2 }, r)

runProgram' :: [N] -> N -> N -> N
runProgram' prog b c =
  head
  . filter (\a -> runProgram prog a b c == prog)
  . map (\a -> if a `mod` 100000 == 0
               then tce "a" a
               else a
        )
  $ [0..]

parseAll :: String -> Input
parseAll input =
  let [regs, prog] = splitOn [""] $ lines input
  in ( map (read @N . drop 1 . dropWhile (/= ':')) $ regs
     , read @[N] . ("[" <>) . (<> "]") . drop 1 . dropWhile (/= ':') $ head prog
     )

part1 :: Input -> [N]
part1 ([a, b, c], prog) = runProgram prog a b c

{-
2,4 ;; B = A & 0b111
1,1 ;; B = B ^ 0b001
7,5 ;; C = A >> B
1,5 ;; B = B ^ 0b101
4,0 ;; B = B ^ C
5,5 ;; out (B & 0b111)
0,3 ;; A = A >> 3
3,0 ;; if A == 0 then end else loop

b1 = a & 0b111
b2 = b1 ^ 0b001
c = a >> b2
b3 = b2 ^ 0b101
b4 = b3 ^ c
out = b4 & 0b111
a = a >> 3

c = a >> b2
  = a >> (b1 ^ 0b001)
  = a >> (low ^ 0b001)

out = b4 & 0b111
    = (b3 ^ c) & 0b111
    = ((b2 ^ 0b101) ^ (a >> (low ^ 0b001))) & 0b111
    = ((low ^ 0b001 ^ 0b101) ^ (a >> (low ^ 0b001))) & 0b111
    = ((low ^ 0b100) ^ (a >> (low ^ 0b001))) & 0b111
-}

part2 :: Input -> N
part2 (_, prog) = head $ go 0b000 (reverse prog)
  where go a =
          \case []     -> [a]
                (x:xs) -> concatMap (\a' -> go a' xs) (nexts a x)
        nexts a t = do
          low <- [0..7]
          let a'  = (a .<<. 3) .|. low
              out = (low .^. 0b100 .^. (a' .>>. (low .^. 0b001))) .&. 0b111
          guard $ out == t
          pure a'

printCommaList :: Show a => [a] -> IO ()
printCommaList = putStrLn . init . tail . show

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   printCommaList (part1 input)
   print (part2 input)
