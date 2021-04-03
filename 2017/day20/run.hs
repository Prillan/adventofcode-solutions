import Control.Applicative
import Data.Foldable
import Data.Ord
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import Data.Ratio
import Data.Void (Void)

newtype V3 a = V3 { asTuple :: (a, a, a) }
  deriving (Show, Eq, Ord)

instance Functor V3 where
  fmap f (V3 (c1, c2, c3)) = V3 (f c1, f c2, f c3)

instance Applicative V3 where
  pure v = V3 (v, v, v)
  (V3 (f1, f2, f3)) <*> (V3 (v1, v2, v3)) = V3 (f1 v1, f2 v2, f3 v3)

instance Num a => Num (V3 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  signum = fmap signum
  abs = fmap abs
  fromInteger = pure . fromInteger

(*.) :: Num a => a -> V3 a -> V3 a
k *. v = fmap (k *) v
{-# SPECIALISE (*.) :: Rational -> V3 Rational -> V3 Rational #-}

manhattan :: Num a => V3 a -> a
manhattan (V3 (x, y, z)) = abs x + abs y + abs z
{-# SPECIALISE manhattan :: V3 Rational -> Rational #-}

data Particle a = Particle { pos :: !(V3 a)
                           , vel :: !(V3 a)
                           , acc :: !(V3 a) }
  deriving (Show, Eq)

instance Functor Particle where
  fmap f (Particle p v a) = Particle (fmap f p) (fmap f v) (fmap f a)

type Parser = Parsec Void String

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

numP :: Parser Integer
numP = do
  sign <- maybe 1 (const $ -1) <$> optional (char '-')
  num <- read <$> some digitChar
  pure $ sign * num

v3P :: Parser (V3 Integer)
v3P = do
  char '<'
  [x, y, z] <- sepBy1 numP (char ',' *> optional spaceChar)
  char '>'
  pure $ V3 (x, y, z)

component :: Char -> Parser (V3 Integer)
component c = char c *> char '=' *> v3P

particleP :: Parser (Particle Integer)
particleP = do
  p <- component 'p'
  string ", "
  v <- component 'v'
  string ", "
  a <- component 'a'
  pure $ Particle p v a

parseAll :: String -> [Particle Rational]
parseAll =
  map (fmap fromInteger) .
  map unsafeRight .
  map (parse particleP "") . lines

ordering :: Num t => Particle t -> (t, t, t)
ordering a = ( manhattan (acc a)
             , manhattan (vel a)
             , manhattan (pos a) )

class Root a where
  sqrt' :: a -> a

checkedSqrt :: (Root t, Num t, Eq t) => t -> Maybe t
checkedSqrt x =
  let r = sqrt' x
    in
      if r * r == x then Just r else Nothing
{-# SPECIALISE checkedSqrt :: Rational -> Maybe Rational #-}

instance Root Float where
  sqrt' = fromInteger . round . sqrt

instance Integral a => Root (Ratio a) where
  sqrt' = rationalSqrt

rationalSqrt :: Integral a => Ratio a -> Ratio a
rationalSqrt x =
  let a' = round $ sqrt (fromIntegral (numerator x) :: Double)
      b' = round $ sqrt (fromIntegral (denominator x) :: Double)
  in
    a' % b'
{-# SPECIALISE rationalSqrt :: Rational -> Rational #-}

data Intersects a = Always
                  | Sometimes [a]
  deriving (Show, Eq)

extractSome :: Intersects t -> [[t]]
extractSome (Sometimes x) = [x]
extractSome _ = []

joinIntersects :: (Eq a, Foldable t) =>
                  t (Intersects a) -> Intersects a
joinIntersects xs =
    let somes = concatMap extractSome xs
        alive = filter (\p -> all (p `elem`) somes) (concat somes)
    in
      case (somes, alive) of
        ([], _) -> Always
        (_, xs') -> Sometimes xs'
{-# SPECIALISE joinIntersects :: [Intersects Rational] -> Intersects Rational #-}

isInteger :: RealFrac a => a -> Bool
isInteger r = fromInteger (round r) == r
{-# SPECIALISE isInteger :: Rational -> Bool #-}

intersection :: (RealFrac a, Root a) =>
                Particle a -> Particle a -> Intersects a
intersection p1 p2 =
  let pd = pos p1 - pos p2
      vd = vel p1 - vel p2
      ad = acc p1 - acc p2

      V3 (tx, ty, tz) = intersection' <$> pd <*> vd <*> ad
  in
    joinIntersects [tx, ty, tz]
{-# SPECIALISE intersection :: Particle Rational -> Particle Rational -> Intersects Rational #-}

intersection' :: (Root a, RealFrac a) =>
                 a -> a -> a -> Intersects a
intersection' pd vd ad
  | pd == 0 && vd == 0 && ad == 0 = Always
  | vd == 0 && ad == 0 = Sometimes []
  | ad == 0 =
    let t = negate pd / vd
    in
      if isInteger t && t >= 0
        then Sometimes [t]
        else Sometimes []
  | otherwise =
    let a = 2 * pd / ad
        b = 1 + 2 * vd / ad

        r = b * b / 4 - a
    in
      case (r >= 0, checkedSqrt r) of
        (True, Just r') ->
          let valid = filter (>= 0)
                . filter isInteger $ [ negate (b/2) + r'
                                     , negate (b/2) - r']
          in if not (null valid)
               then Sometimes valid
               else Sometimes []
        _ -> Sometimes []
{-# SPECIALISE intersection' ::
    Rational -> Rational -> Rational -> Intersects Rational #-}

position :: Fractional a => Particle a -> a -> V3 a
position p t = pos p + (t *. vel p) + ((t * (t + 1) / 2) *. acc p)
{-# SPECIALISE position :: Particle Rational -> Rational -> V3 Rational #-}

part1 :: [Particle Rational] -> Int
part1 = fst . minimumBy (comparing (ordering . snd)) . zip [0..]

part2 :: [Particle Rational] -> Int
part2 input =
  let intersects p1 p2 =
        case intersection p1 p2 of
          Sometimes [] -> []
          Always -> [0]
          Sometimes xs -> [minimum xs]

      points :: [(Int, Particle Rational)]
      points = zip [0..] input
      intersections = Set.fromList $ concat [intersects p q | (pi, p) <- points
                                                            , (qi, q) <- points
                                                            , pi < qi]

      alone t remaining (_, p) =
        let pPos = position p t
            c = length $ filter (\(_, q) -> position q t == pPos) remaining
        in c <= 1
      reduce remaining t =
        filter (alone t remaining) remaining
  in
    length $ foldl reduce points (sort $ toList intersections)

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
