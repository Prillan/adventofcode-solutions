{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.String (fromString)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

type S = ByteString
type Input = ([S], [S])

parseTargets :: String -> [S]
parseTargets = map fromString . lines

parseTowels :: String -> [S]
parseTowels =
  map fromString
  . filter (not . null)
  . map (filter (\c -> 'a' <= c && c <= 'z'))
  . splitOn ","

parseAll :: String -> Input
parseAll input =
  let [towels, targets] = splitOn "\n\n" input
  in (parseTowels towels, parseTargets targets)

search :: [S] -> S -> Int
search towels t = go t
  where memoized = HashMap.fromList $ map (\x -> (x, go x)) $ BS.tails t
        go = \case x | BS.null x -> 1
                     | otherwise -> sum $ map (memoized HashMap.!) (matching x)
        matching x = mapMaybe (`BS.stripPrefix` x) towels

parts :: Input -> [Int]
parts (towels, targets) =
  let counts = map (search towels) targets
  in [ length (filter (> 0) counts)
     , sum counts
     ]

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   mapM_ print (parts input)
