{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
import Control.Applicative ((<|>))

import Data.Char (isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

type N = Int

digits :: (String -> Maybe Char) -> String -> [Char]
digits f = mapMaybe f . tails

digit :: String -> Maybe Char
digit = \case
  x:_ | isDigit x -> Just x
  _ -> Nothing

wordDigit :: String -> Maybe Char
wordDigit = \case
  x
    | "one" `isPrefixOf` x -> Just '1'
    | "two" `isPrefixOf` x -> Just '2'
    | "three" `isPrefixOf` x -> Just '3'
    | "four" `isPrefixOf` x -> Just '4'
    | "five" `isPrefixOf` x -> Just '5'
    | "six" `isPrefixOf` x -> Just '6'
    | "seven" `isPrefixOf` x -> Just '7'
    | "eight" `isPrefixOf` x -> Just '8'
    | "nine" `isPrefixOf` x -> Just '9'
  _ -> Nothing

extract :: [Char] -> N
extract xs = read [head xs, last xs]

solve :: (String -> Maybe Char) -> [String] -> N
solve f = sum . map extract . map (digits f)

part1 :: [String] -> N
part1 = solve digit

part2 :: [String] -> N
part2 = solve \x -> digit x <|> wordDigit x

main :: IO ()
main = main' "input.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- lines <$> readFile file
   print (part1 input)
   print (part2 input)
