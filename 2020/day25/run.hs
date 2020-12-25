{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.List (findIndex, iterate')

parseAll :: String -> (Int, Int)
parseAll s =
  let [cardPub, doorPub] = map (read @Int)  (lines s)
  in (cardPub, doorPub)

transforms :: [Int]
transforms = iterate' (\n -> (n * 7) `mod` 20201227) 1

modExp :: Int -> Int -> Int -> Int
modExp b e n = go 1 b e
  where go r _  0 = r
        go r b' e'
          | e' `mod` 2 == 0 = go r (b' * b' `mod` n) (e' `div` 2)
          | otherwise       = go (r * b' `mod` n) (b' * b' `mod` n) (e' `div` 2)

part1 :: (Int, Int) -> Int
part1 (cardPub, doorPub) =
  let Just anyExp = findIndex (\x -> x == cardPub || x == doorPub) transforms
      base = if transforms !! anyExp == cardPub
             then doorPub
             else cardPub
  in modExp base anyExp 20201227

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
