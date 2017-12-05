import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M

parse :: String -> IntMap Int
parse = M.fromList . zip [0..] . map read . lines

isNothing Nothing = True
isNothing _ = False

part1 input =
  fst . head . filter (isNothing . snd) . zip [-1..] $ iterate go (Just (0, input))
  where go (Just (ip, m)) = do
            offset <- M.lookup ip m
            pure (ip + offset, M.insert ip (offset + 1) m)
        go Nothing = Nothing

part2 input =
  fst . head . filter (isNothing . snd) . zip [-1..] $ iterate go (Just (0, input))
  where go (Just (ip, m)) = do
            offset <- M.lookup ip m
            let s = if offset >= 3 then -1 else 1
            pure (ip + offset, M.insert ip (offset + s) m)
        go Nothing = Nothing

main = do
  input <- parse <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
