{-# LANGUAGE TypeApplications #-}
import Data.List (findIndices, sort, elemIndices)
import Data.List.Split (chunksOf)

import Control.Applicative ((<|>))

import Text.Read (readListPrec, readPrec)

data Tree = Leaf Int
          | Branch [Tree]
  deriving (Eq, Show)

instance Read Tree where
  readPrec = (Leaf <$> readPrec @Int)
             <|> (Branch <$> readListPrec @Tree)

instance Ord Tree where
  compare t1 t2 =
    case (t1, t2) of
      (Leaf n1, Leaf n2)     -> compare n1 n2
      (Branch x1, Branch x2) -> compare x1 x2
      (Leaf n1, Branch x2) -> compare [Leaf n1] x2
      (Branch x1, Leaf n2) -> compare x1 [Leaf n2]

parse :: String -> Tree
parse = read

parseAll :: String -> [Tree]
parseAll =
  map parse
  . filter (not . null)
  . lines

part1 :: [Tree] -> Int
part1 =
  sum
  . map (+1)
  . elemIndices LT
  . map (\[x1, x2] -> compare x1 x2)
  . chunksOf 2

dividers :: [Tree]
dividers = map read
  [ "[[2]]"
  , "[[6]]"
  ]

part2 :: [Tree] -> Int
part2 = product . map (+1) . findIndices (`elem` dividers) . sort . (<> dividers)

main :: IO ()
main = main' "input.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
