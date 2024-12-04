{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import Data.Functor (void)
import Data.Maybe (listToMaybe)

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

type N = Integer

mulP :: ReadP (N, N)
mulP = do
  _ <- string "mul"
  between (string "(") (string ")") do
    i1 <- intP
    _ <- char ','
    i2 <- intP
    pure (i1, i2)

doP :: ReadP ()
doP   = void $ string "do()"

dontP :: ReadP ()
dontP = void $ string "don't()"

intP :: ReadP N
intP = read @N <$> munch1 isDigit

parse :: ReadP a -> String -> Maybe (a, String)
parse p = listToMaybe . readP_to_S p

type Op a = N -> N -> a

eval :: Op a -> Op a -> String -> [a]
eval on off = go on
  where go f = \case
          [] -> []
          xs
            | Just ((i1, i2), xs') <- parse mulP xs  -> f i1 i2:go f   xs'
            | Just (_, xs')        <- parse doP xs   ->         go on  xs'
            | Just (_, xs')        <- parse dontP xs ->         go off xs'
          _:xs -> go f xs

part1 :: String -> N
part1 = sum . eval (*) (*)

part2 :: String -> N
part2 = sum . eval (*) (\_ _ -> 0)

main :: IO ()
main = main' "input.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- readFile file
   print (part1 input)
   print (part2 input)
