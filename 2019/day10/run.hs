{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
import Data.Foldable (toList)
import Control.Arrow ((&&&))
import Control.Applicative (liftA2)
import Data.Ratio (numerator)
import Data.Ord (comparing)
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

newtype Pair a = Pair (a, a)
  deriving (Eq, Show, Ord, Functor)

instance Applicative Pair where
  pure x = Pair (x, x)
  Pair (f, g) <*> Pair (x, y) = Pair (f x, g y)

instance Num a => Num (Pair a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = fmap fromInteger . pure

instance Fractional a => Fractional (Pair a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = fmap fromRational . pure

type Position = Pair Rational
type Vec = Pair Rational

parseAll :: String -> [String]
parseAll = lines

readPositions :: (Ord a, Enum a, Num a) => [[Char]] -> Set (Pair a)
readPositions grid =
  Set.fromList $ [Pair (ci, ri) | (ri, r) <- zip [0..] grid
                                , (ci, c) <- zip [0..] r
                                , c == '#' ]

safeDiv :: (Fractional a, Eq a) => a -> a -> Maybe a
safeDiv x y
  | y == 0 = Nothing
  | otherwise = Just $ x / y

collision :: Position -> Vec -> Position -> Maybe Rational
collision base dir other =
  case safeDiv <$> (other - base) <*> dir of
    Pair (Nothing, Nothing) -> Nothing
    Pair (Just tx, Nothing) -> if base + fromRational tx * dir == other
                                  then Just tx
                                  else Nothing
    Pair (Nothing, Just ty) -> if base + fromRational ty * dir == other
                                  then Just ty
                                  else Nothing
    Pair (Just tx, Just ty) -> if tx == ty then Just tx else Nothing

visible :: Pair Rational
  -> Pair Rational
  -> Set (Pair Rational)
  -> Bool
visible base target others =
  let dir = target - base
      blocksTarget x =
        case collision base dir x of
          Just t | 0 < t && t < 1 -> True
          _ -> False
  in
    all (not . blocksTarget)
    . filter (/= target)
    . filter (/= base) $ toList others

sees :: Position -> Set Position -> Set Position
sees base others =
  Set.filter (\x -> visible base x others)
  . Set.filter (/= base) $ others

bestAsteroid :: Set Position -> (Int, Position)
bestAsteroid asteroids =
  maximumBy (comparing fst)
  . map (length . flip sees asteroids &&& id)
  $ toList asteroids


next :: (Foldable t, RealFloat a)
  => a
  -> Position
  -> t Position
  -> Position
next theta base asteroids =
  minimumBy (nextComp theta base) asteroids

nextComp :: RealFloat a => a -> Position -> Position -> Position -> Ordering
nextComp theta base p1 p2
   | comp == EQ = comparing (distance base) p1 p2
   | otherwise  = comp
   where comp = comparing (\x -> unitCircle $ angle (x - base) - theta) p1 p2

unitCircle :: (Ord a, Floating a) => a -> a
unitCircle phi
  | phi < 0      = phi + 2 * pi
  | phi > 2 * pi = phi - 2 * pi
  | otherwise    = phi

distance :: Num a => Pair a -> Pair a -> a
distance (Pair (x0, y0)) (Pair (x1, y1)) =
    abs (x1 - x0) + abs (y1 - y0)

angle :: RealFloat a => Position -> a
angle (Pair (x, y))
  | theta < 0      = theta + 2 * pi
  | otherwise      = theta
  where theta = atan2 (fromRational y) (fromRational x)


-- TODO: Simplify by creating a sorted list of [Position] using angle
-- <|> distance, then group them on angle, followed by a cyclic
-- popping of elements in the list.
vaporize :: Position -> Set Position -> [Position]
vaporize base others =
  let initial = let Pair (x, _) = base in Pair (x - 1, -1)
      others' = Set.delete base others

      step :: (Position, Double, Set Position, [Position])
           -> (Position, Double, Set Position, [Position])
      step (lastDestroyed, phi, remaining, acc)
        | Set.null remaining = (lastDestroyed, phi, remaining, acc)
        | otherwise =
          let valid = Set.filter (not . isJust . collision base (lastDestroyed - base))
                      $ remaining
              next' =
                if Set.null valid
                  then minimumBy (comparing (distance base)) remaining
                  else next phi base valid
          in
            ( next'
            , angle (next' - base)
            , Set.delete next' remaining
            , acc ++ [next'] )
  in
    head
    . map (\(_, _, _, result) -> result)
    . filter (\(_, _, remaining, _) -> Set.null remaining)
    . drop 1
    $ iterate step ( initial
                   , angle (initial - base)
                   , others'
                   , [] )

part2 :: Position -> Set Position -> Integer
part2 base asteroids =
  let Pair (x, y) = numerator <$> (vaporize base asteroids) !! 199
  in
    x * 100 + y

main :: IO ()
main = do
   input <- readPositions . parseAll <$> readFile "input.txt"
   let (numVisible, best) = bestAsteroid input
   print numVisible
   print (part2 best input)
