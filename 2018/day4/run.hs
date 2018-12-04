{-# Language OverloadedStrings #-}
import Data.Ord
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec (ErrorItem Char) String

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

type Time = Int
type GuardId = Int
data Event = ShiftStart GuardId
           | FallsAsleep Time
           | WakesUp Time
  deriving (Show, Eq)

int :: Parser Int
int = read <$> many digitChar

timep = do
  "[1518-" *> some digitChar *> "-" *> some digitChar *> " "
  hour <- int
  ":"
  minute <- int
  "]"
  pure minute

eventp = do
  time <- timep
  ShiftStart <$> (" Guard #" *> int <* " begins shift")
    <|> FallsAsleep time <$ " falls asleep"
    <|> WakesUp time <$ " wakes up"

type Counter a = Map a Int

counter :: Ord a => [a] -> Counter a
counter = Map.fromListWith (+) . flip zip (repeat 1)

parseAll =
  map unsafeRight .
  map (parse eventp "") . filter (not . null) . sort . lines

guardAsleepTimes events =
  let updateGuardTimes m gid ft wt =
        Map.unionWith (Map.unionWith (+))
                      m
                      (Map.singleton gid (counter [ft..wt-1]))

      go (m,        _,       _) (ShiftStart gid) = (m, Just gid, Nothing)
      go (m, Just gid, Nothing) (FallsAsleep t)    = (m, Just gid, Just t)
      go (m, Just gid, Just ft) (WakesUp wt)  =
        (updateGuardTimes m gid ft wt, Just gid, Nothing)
      go _ _ = undefined

      (times, _, _) = foldl go (Map.empty, Nothing, Nothing) events
  in
    times

values :: Map a b -> [b]
values = map snd . Map.toList

part1 input = id * t
  where (id, asleep) = maximumBy (comparing (sum . values . snd))
                       . Map.toList
                       . guardAsleepTimes $ input
        (t, _) = maximumBy (comparing snd) (Map.toList asleep)

thd :: (a, b, c) -> c
thd (_, _, z) = z

flatten :: Map a (Map b c) -> [(a, b, c)]
flatten m = do
  (x, vals) <- Map.toList m
  (y, z) <- Map.toList vals
  pure (x, y, z)

part2 input = id * t
  where (id, t, _) = maximumBy (comparing thd)
                     . flatten
                     . guardAsleepTimes $ input

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
