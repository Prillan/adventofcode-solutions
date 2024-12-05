{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Data.Function ((&))
import Data.List (foldl', mapAccumL)
import Data.List.Split (splitOn, splitOneOf)
import Data.Char (ord)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type N = Int

data Op = Set N | Remove
  deriving Show

type Instr = (String, N, Op)

type Input = [(String, Instr)]

parse :: String -> Instr
parse input = splitOneOf "=-" input & \case
  [label, ""] -> (label, hash label, Remove)
  [label, v]  -> (label, hash label, Set (read v))

parseAll :: String -> Input
parseAll = map (\x -> (x, parse x)) . splitOn "," . filter (/= '\n')

hash :: String -> N
hash = go 0
  where go v [] = v
        go v (x:xs) =
          go ((17 * (v + ord x)) `mod` 256) xs

part1 :: Input -> N
part1 = sum . map (hash . fst)

type Box = [(String, N)]

remove :: String -> Maybe Box -> Maybe Box
remove l = fmap (filter ((/= l) . fst))

set :: String -> N -> Maybe Box -> Maybe Box
set l v b = Just $ case mapAccumL f False (maybe [] id b) of
                     (True,  b') -> b'
                     (False, b') -> (l, v):b'
  where f True  x = (True, x)
        f False x@(l', _) | l == l'   = (True, (l, v))
                          | otherwise = (False, x)

lenses :: [Instr] -> HashMap N Box
lenses = foldl' f HashMap.empty
  where f m = \case
          (l, h, Remove) -> HashMap.alter (remove l) h m
          (l, h, Set v ) -> HashMap.alter (set l v)  h m

part2 :: Input -> N
part2 =
  sum
  . concatMap (\(bi, box) -> zipWith (\i (_, v) -> (bi + 1) * i * v) [1..] $ reverse box)
  . HashMap.toList
  . lenses
  . map snd

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
