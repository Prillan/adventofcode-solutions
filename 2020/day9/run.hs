{-# LANGUAGE TypeApplications #-}
import Data.Bifunctor (second)
import Data.Maybe
import Data.List

parseAll =
  map (read @Int) . lines

sequences n nums =
  let (h, t) = splitAt n nums
  in
    zip t $ scanl (\curr x -> drop 1 curr ++ [x]) h t

sums xs = [x + y | x <- xs, y <- xs, x /= y]

diffs xs = zipWith (-) (drop 1 xs) xs

findSum t =
  diffs
  . head
  . filter ((== t) . last)
  . filter ((>= 2) . length)
  . map (takeWhile (<= t) . scanl (+) 0)
  . tails

part1 =
  fst
  . head
  . filter (not . uncurry elem)
  . map (second sums)
  . sequences 25
part2 input =
  let target = part1 input
      s = findSum target input
  in minimum s + maximum s

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
