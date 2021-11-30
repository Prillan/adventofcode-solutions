import           Data.Foldable (toList)
import           Data.List (splitAt)
import           Data.Sequence (Seq, ViewL(..), (|>), (<|))
import qualified Data.Sequence as Seq

step [] = []
step [x] = [x]
step (x:y:xs) = x:step xs

step' (l, xs)
  | odd l = ((l-1) `div` 2, drop 1 $ step xs)
  | otherwise = (l `div` 2, step xs)

input = 3012210

single [x] = True
single _ = False

part1 :: Int
part1 = head
        . snd
        . until (single . snd) step'
        $ (input, [1..input])

step2 seq =
  case (Seq.viewl seq) of
    EmptyL    -> error ""
    (x :< xs) ->
      let l = Seq.length seq
          i = (l `div` 2) - 1
      in
        (Seq.deleteAt i xs) |> x

single' = (== 1).Seq.length

part2 :: Int
part2 = head
        . toList
        . until single' step2
        $ Seq.fromList [1..input]

main = do
   print part1
   print part2
