{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Control.Applicative (Applicative(..), liftA2)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

parseAll :: String -> Map (V2 Int) Char
parseAll = Map.fromList
  . concatMap (\(li, l) -> zipWith (\ri c -> (V2 (li, ri), c)) [0..] l)
  . zip [0..]
  . lines

newtype V2 a = V2 { asTuple :: (a, a) }
  deriving (Show, Eq, Ord)

instance Functor V2 where
  fmap f (V2 (c1, c2)) = V2 (f c1, f c2)

instance Applicative V2 where
  pure v = V2 (v, v)
  (V2 (f1, f2)) <*> (V2 (v1, v2)) = V2 (f1 v1, f2 v2)

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  signum = fmap signum
  abs = fmap abs
  fromInteger = pure . fromInteger

lineFrom :: Num a => a -> a -> [a]
lineFrom dir = drop 1 . iterate (+ dir)

pointIs :: Ord k => (t -> Bool) -> Map k t -> k -> Bool
pointIs p m pos =
    case Map.lookup pos m of
      Just x  -> p x
      Nothing -> False

isStop :: Ord k => Map k Char -> k -> Bool
isStop grid x = case Map.lookup x grid of
                  Just '+' -> True
                  Just c -> c `elem` ['A'..'Z']
                  Nothing -> True

step :: (Ord a, Num a)
     => Map (V2 a) Char
     -> (V2 a, V2 a, Seq Char, Int)
     -> Either (V2 a, V2 a, Seq Char, Int) (Seq Char, Int)
step grid (dir, pos, seen, steps) =
  let segment = pos:(takeWhile (not.isStop grid) $ lineFrom dir pos)
      dirCands = [d | d <- [V2 (0, 1), V2 (1, 0), V2 (-1, 0), V2 (0, -1)],
                   d /= negate dir ]
  in
    case map (+dir) (reverse segment) of
      pos':_ ->
        let dirs = filter (pointIs (/= ' ') grid . (pos' +)) $ dirCands
            seen' = case Map.lookup pos' grid of
                Just '+' -> seen
                Just c   -> seen |> c
                Nothing  -> seen
            steps' = steps + length segment
        in
          case dirs of
            dir':_ -> Left (dir', pos', seen', steps')
            _ -> Right (seen', steps')
      _ -> Right (seen, steps)

trace :: (Ord a, Num a)
      => Map (V2 a) Char
      -> [Either (V2 a, V2 a, Seq Char, Int) (Seq Char, Int)]
trace grid =
  let (start, _):_ = filter ((== '|') . snd)
                   . filter ((== 0).fst.asTuple.fst) $ Map.toList grid
      step' (Left x) = step grid x
      step' (Right x) = Right x
  in
    iterate step' (Left (V2 (1, 0), start, Seq.empty, 1))

firstRight :: [Either a b] -> b
firstRight [] = error "Whoops"
firstRight (Left _:xs) = firstRight xs
firstRight (Right v:_) = v

pathStats :: Map (V2 Int) Char -> (String, Int)
pathStats = first toList . firstRight . trace

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   let (seen, steps) = pathStats input
   putStrLn $ seen
   putStrLn $ show steps
