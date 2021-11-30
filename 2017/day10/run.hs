import Data.Bits (xor)
import Data.Semigroup ((<>))
import Data.Foldable (toList)
import Data.Char (intToDigit, ord)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

shift :: Int -> Seq a -> Seq a
shift i s =
  let (first, second) = Seq.splitAt i s
  in second <> first

unshift :: Int -> Seq a -> Seq a
unshift i s = shift (Seq.length s - i) s

hash :: Int -> [Int] -> Seq Int
hash l input =
  let (finalPos, _, nums) = foldl (step l) (0, 0, Seq.fromList [0..l-1]) input
  in
    unshift (finalPos `mod` l) nums

step :: Int -> (Int, Int, Seq Int) -> Int -> (Int, Int, Seq Int)
step l (pos, skipSize, nums) current =
  let (sublist, rest) = Seq.splitAt current nums
      sublist' = Seq.reverse sublist
  in
    ( (pos + current + skipSize) `mod` l
    , skipSize + 1
    , shift ((skipSize + current) `mod` l) (sublist' <> rest))

part1 :: [Int] -> Int
part1 input =
  let n1:n2:_ = toList $ hash 256 input
  in
    n1 * n2

hex :: Int -> String
hex x = [intToDigit (x `div` 16), intToDigit (x `mod` 16)]

suffix :: [Int]
suffix = [17, 31, 73, 47, 23]

part2 :: String -> String
part2 input =
  let input' = map ord input
      h = hash 256 $ concat $ replicate 64 (input' ++ suffix)
  in
    concatMap (hex . foldl xor 0 . toList) $ toList $ Seq.chunksOf 16 h

main :: IO ()
main = do
  input <- filter (/= '\n') <$> readFile "input.txt"
  print $ part1 . read $ "[" ++ input ++ "]"
  putStrLn $ part2 input
