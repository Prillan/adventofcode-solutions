import Data.Semigroup ((<>))
import Data.Foldable (toList)
import Data.Char (digitToInt, intToDigit, ord)
import Data.Bits (xor, popCount, testBit)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

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

hex :: Int -> String
hex x = [intToDigit (x `div` 16), intToDigit (x `mod` 16)]

suffix :: [Int]
suffix = [17, 31, 73, 47, 23]

knotHash :: String -> String
knotHash input =
  let input' = map ord input
      h = hash 256 $ concat $ replicate 64 (input' ++ suffix)
  in
    concatMap (hex . foldl xor 0 . toList) $ toList $ Seq.chunksOf 16 h

hashPopCount :: String -> Int
hashPopCount = sum . map (popCount . digitToInt)

part1 :: [String] -> Int
part1 = sum . map hashPopCount

hexToBits :: Char -> [Bool]
hexToBits h = map (testBit (digitToInt h)) [3,2..0]

hashBits :: String -> [Bool]
hashBits = concatMap hexToBits

gridFromBits :: [[Bool]] -> Set (Int, Int)
gridFromBits = Set.fromList
  . map fst
  . filter snd
  . concatMap (\(r, bs) -> zipWith (\c b -> ((c, r), b)) [0..] bs)
  . zip [0..]

gridFromHashes :: [String] -> Set (Int, Int)
gridFromHashes = gridFromBits . map hashBits

groups :: Set (Int, Int) -> [Set (Int, Int)]
groups s = groups' (toList s)
  where groups' [] = []
        groups' (c:cs) =
          let (checked, g) = findAll s c
          in
            g:groups' (filter (not . flip Set.member checked) cs)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) =
  [ (x + 1, y    )
  , (x - 1, y    )
  , (x    , y + 1)
  , (x    , y - 1) ]


findAll :: Set (Int, Int) -> (Int, Int) -> (Set (Int, Int), Set (Int, Int))
findAll s c = findAll' [c] (Set.empty) (Set.singleton c)
  where findAll' [] checked g = (checked, g)
        findAll' (x:xs) checked g
          | x `Set.member` checked = findAll' xs checked g
          | x `Set.member` s =
              findAll' (neighbours x ++ xs)
                       (Set.insert x checked)
                       (Set.insert x g)
          | otherwise = findAll' xs (Set.insert x checked) g

part2 :: [String] -> Int
part2 = length . groups . gridFromHashes

main = do
  let input = "ffayrhll"
      hashes = map (knotHash . ((input ++ "-") ++) . show) [0..127]
  print (part1 hashes)
  print (part2 hashes)
