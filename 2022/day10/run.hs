{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Draw.Chars (readLetters)

import Data.List.Split (chunksOf)

parse :: [String] -> [Int]
parse =
  \case ["addx", d] -> [0, read @Int d]
        _ -> [0]

parseAll :: String -> [Int]
parseAll = concatMap (parse . words)  . lines

part1 :: [Int] -> Int
part1 input =
  let values = scanl (+) 1 input
      ixs = [20, 60, 100, 140, 180, 220]
  in
    sum $ map (\ix -> ix * values !! (ix - 1)) ixs

part2 :: [Int] -> Either String String
part2 input =
  let spritePos = init $ scanl (+) 1 input
      pos = cycle [0..39]
      draw p sp
        | abs (p - sp) <= 1 = '#'
        | otherwise         = '.'
  in
    readLetters
    . unlines
    . chunksOf 40
    $ zipWith draw pos spritePos

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   case part2 input of
     Right x -> putStrLn x
     Left e -> putStrLn e
