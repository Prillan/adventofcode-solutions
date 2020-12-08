parseAll =
  map parse . lines

parse :: String -> (Int, Int, Char, String)
parse input = head $ do
  (low, rest) <- reads input
  (high, rest') <- reads (drop 1 rest)
  let ' ':c:':':' ':rest'' = rest'
  pure (low, high, c, rest'')

valid1 (l, h, c, pw) =
  let count = length . filter (== c) $ pw
  in l <= count && count <= h

part1 = length . filter valid1

valid2 (l, h, c, pw) =
  let lc = pw !! (l - 1)
      hc = pw !! (h - 1)
  in
    (lc == c && hc /= c) || (lc /= c && hc == c)

part2 = length . filter valid2

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
