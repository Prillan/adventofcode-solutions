{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
import Text.Read

type Interval = (Int, Int)

-- Kinda ugly, wanted to try out Text.Read stuff.
parse :: String -> (Interval, Interval)
parse = fst . head . flip readPrec_to_S 0 do
  ll <- readPrec @Int
  _ <- get
  lh <- readPrec @Int
  _ <- get
  rl <- readPrec @Int
  _ <- get
  rh <- readPrec @Int
  pure ((ll, lh), (rl, rh))

parseAll :: String -> [(Interval, Interval)]
parseAll = map parse  . lines

contains :: Interval -> Interval -> Bool
contains (ll, lh) (rl, rh) =
  ll <= rl && rh <= lh

overlaps :: Interval -> Interval -> Bool
overlaps (ll, lh) (rl, rh) =
  (ll <= rl && rl <= lh)
  || (rl <= ll && ll <= rh)

part1 :: [(Interval, Interval)] -> Int
part1 = length . filter (\(i1, i2) -> i1 `contains` i2 || i2 `contains` i1)

part2 :: [(Interval, Interval)] -> Int
part2 = length . filter (uncurry overlaps)

main :: IO ()
main = main' "input.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
