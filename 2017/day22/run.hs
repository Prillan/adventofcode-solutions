{-# LANGUAGE BangPatterns #-}
import Control.Applicative (liftA2, some, (<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Megaparsec (Parsec, Dec, char, parse)

type Parser = Parsec Dec String

rowP :: Parser [Bool]
rowP = some $ (char '.' *> pure False) <|> (char '#' *> pure True)

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

type Point = V2 Int

newtype Grid = Grid (Map Point NodeState)
  deriving (Show)

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

parseAll :: String -> (Int, Grid)
parseAll input =
  let lgrid = map unsafeRight
        . map (parse rowP "")
        . lines
        $ input
      sgrid = Grid
        . Map.fromList
        . concatMap (\(li, l) -> zipWith (\ri c -> (V2 (li, ri), fromBool c)) [0..] l)
        . zip [0..]
        $ lgrid
  in
    (length lgrid, sgrid)

type VirusState = (Point, Point, Grid, Int)

data NodeState = Clean | Weakened | Infected | Flagged
  deriving (Show, Eq)

fromBool :: Bool -> NodeState
fromBool True = Infected
fromBool False = Clean

turnRight, turnLeft, turnBack :: Point -> Point
turnRight (V2 (r, c)) = V2 (c, -r)
turnLeft (V2 (r, c)) = V2 (-c, r)
turnBack = turnRight . turnRight

state :: Point -> Grid -> NodeState
state p (Grid g) = Map.findWithDefault Clean p g

infected :: Point -> Grid -> Bool
infected pos g = state pos g == Infected

updateNode :: Point -> NodeState -> Grid -> Grid
updateNode p s (Grid g) = Grid $ Map.insert p s g

burst1 :: VirusState -> VirusState
burst1 (!pos, !dir, !grid, !count) =
  let currentInfected = infected pos grid
      dir' = if currentInfected
               then turnRight dir
               else turnLeft dir
      current' = if currentInfected
                    then Clean
                    else Infected
      pos' = pos + dir'
      count' = count + (if currentInfected then 0 else 1)
      grid' = updateNode pos current' grid
  in
    (pos', dir', grid', count')

burst2 :: VirusState -> VirusState
burst2 (!pos, !dir, !grid, !count) =
  let current = state pos grid
      (dir', current') =
        case current of
          Clean    -> (turnLeft dir , Weakened)
          Weakened -> (dir          , Infected)
          Infected -> (turnRight dir, Flagged )
          Flagged  -> (turnBack dir , Clean   )

      pos' = pos + dir'
      count' = count + (if current' == Infected then 1 else 0)
      grid' = updateNode pos current' grid
  in
    (pos', dir', grid', count')

iterate' :: Int -> (a -> a) -> a -> a
iterate' 0 _ x = x
iterate' n f x =
  let !x' = f x
  in
    iterate' (n - 1) f x'

runFor :: Int -> (VirusState -> VirusState) -> (Int, Grid) -> VirusState
runFor n burstMode (width, grid) =
  iterate' n burstMode (V2 (width `div` 2, width `div` 2), V2 (-1, 0), grid, 0)

part1 :: (Int, Grid) -> Int
part1 = (\(_, _, _, c) -> c) . runFor 10000 burst1
part2 :: (Int, Grid) -> Int
part2 = (\(_, _, _, c) -> c) . runFor 10000000 burst2

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print $ part1 input
   print $ part2 input
