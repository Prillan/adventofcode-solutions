{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Control.Applicative
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

type Row = ([Char], [N])

parse input =
  let [cs, checksum] = words input
  in
    (cs, read @[N] $ "[" <> checksum <> "]")

parseAll = map parse  . lines

solve :: Row -> [[Char]]
solve (initial, checksum) = go initial checksum
  where  go [] [] = pure []
         go [] _  = empty
         go ('.':xs) cs = ('.':) <$> go xs cs
         go ('#':_)  [] = empty
         go "#" [1] = pure "#"
         go ('#':xs) (c:cs) =
           case consumeBlock (c - 1) xs of
             Just rest -> (replicate c '#' ++) <$> go rest cs
             Nothing   -> empty
         go ('?':xs) cs =
           go ('#':xs) cs <|> go ('.':xs) cs
         go xs cs = error $ unlines [ "xs: " <> show xs
                                    , "cs: " <> show cs
                                    ]

solve' (initial, checksum) = go initial (length initial) checksum (sum checksum)
  where  go [] _ [] _ = 1
         go [] _ _ _ = 0
         go _  r _ cr
           | r < cr = 0
         go xs _ [] _
           | any (== '#') xs = 0
           | otherwise       = 1
         go ('.':xs) r cs cr = go xs (r - 1) cs cr
         go "#" _ [1] _ = 1
         go ('#':xs) r (c:cs) cr =
           case consumeBlock (c - 1) xs of
             Just rest -> go rest (r - c) cs (cr - c)
             Nothing   -> 0
         go ('?':xs) r cs cr =
           go ('#':xs) r cs cr + go ('.':xs) r cs cr
         go xs r cs cr = error $ unlines [ "xs: " <> show xs
                                    , "cs: " <> show cs
                                    ]

consumeBlock c xs = go 0 xs
  where go 0 [] = Nothing
        go i []
          | i == c    = Just []
          | otherwise = Nothing
        go i ('#':rest)
          | i  < c = go (i + 1) rest
          | i == c = Nothing
        go i ('.':rest)
          | i == c    = Just ('.':rest)
          | otherwise = Nothing
        go i ('?':rest)
          | i  < c = go (i + 1) rest
          | i == c = Just ('.':rest)


extend (initial, checksum) = (r initial, r checksum)
  where r = concat . replicate 5

part1 =
  sum . map solve'
part2 =
  map (solve' . extend)

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   mapM_ print (part2 input)
