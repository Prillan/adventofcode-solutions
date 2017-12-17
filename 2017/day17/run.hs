{-# LANGUAGE BangPatterns #-}
import Data.Maybe (maybe)
import Data.Sequence (Seq, (!?))
import qualified Data.Sequence as Seq

buildBuffer :: Int -> [(Int, Int, Seq Int)]
buildBuffer step = iterate (buildBuffer' step) (0, 1, Seq.singleton 0)

buildBuffer' :: Int -> (Int, Int, Seq Int) -> (Int, Int, Seq Int)
buildBuffer' step (pos, l, buf) =
  let i = (pos + step) `mod` l
  in
    (i + 1, l + 1, Seq.insertAt (i + 1) l buf)

part1 :: Int -> Int
part1 input =
  let (pos, _, buf) = buildBuffer input !! 2017
  in
    maybe (error "Whoops") id (buf !? (pos + 1))

afterZero :: Int -> Int -> Int
afterZero step n = afterZero' step (n - 1) 1 2 1

afterZero' :: Int -> Int -> Int -> Int -> Int -> Int
afterZero' _ 0 _ _ !val = val
afterZero' step n pos l !val =
  let i = (pos + step) `mod` l
      val' = if i == 0 then l else val
  in
    afterZero' step (n - 1) (i + 1) (l + 1) val'

part2 :: Int -> Int
part2 input = afterZero input 50000000

main = do
  let input = 335
  print (part1 input)
  print (part2 input)
