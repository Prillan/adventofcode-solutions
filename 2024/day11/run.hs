{-# LANGUAGE LambdaCase #-}
import AoC

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type N = Int

parseAll :: String -> [N]
parseAll = map (read @N) . words

single :: N -> [N]
single = \case
  x | x == 0 -> [1]
    | s <- show x
    , l <- length s
    , even l -> let (h, t) = splitAt (l `div` 2) s
                in [ read h
                   , read t
                   ]
    | otherwise -> [x * 2024]

step :: IntMap Int -> IntMap Int
step = IntMap.unionsWith (+) . map go . IntMap.toList
  where go (k, v) = IntMap.fromListWith (+) . map (, v) $ single k

solve :: Int -> [N] -> Int
solve steps = sum . map snd . IntMap.toList . iterateN steps step . IntMap.fromListWith (+) . map (, 1)

part1 :: [N] -> Int
part1 = solve 25

part2 :: [N] -> Int
part2 = solve 75

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
