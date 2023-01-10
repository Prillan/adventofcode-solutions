{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
import AoC
import AoC.Grid
import AoC.Parse (numP)

import Control.Monad (guard)
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void

-- TODO: Clean up

type Parser a = Parsec Void String a
type N = Int
type Grid = HashMap (V2 N) Char

data Instr = Move N
           | TurnLeft
           | TurnRight
  deriving (Show, Eq)

pathP :: Parser [Instr]
pathP = many instrP

instrP :: Parser Instr
instrP =
  choice
  [ Move <$> numP
  , TurnLeft <$ char 'L'
  , TurnRight <$ char 'R'
  ]

parseAll :: String -> (Grid, ((N,N),(N,N)), [Instr])
parseAll input =
  let [grid, path] = splitOn "\n\n" input
      g = HashMap.filter (`elem` ".#") $ parseMapGrid id grid
      (xs, ys) = unzip $ HashMap.keys g
      g' = HashMap.mapKeys v2 g
      bounds = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  in (g', bounds, (\(Right x) -> x) $ parse pathP "" path)

singleStep :: Grid -> ((N, N), (N, N)) -> Dir -> V2 N -> V2 N
singleStep g ((xmin, ymin), (xmax, ymax)) dir pos@(V2 (px, py)) =
  case g HashMap.!? (pos + dirv dir) of
    Just '.' -> pos + dirv dir
    Just '#' -> pos
    Nothing  ->
      let candidate =
            case dir of
              R -> until (`HashMap.member` g) (+ dirv dir) (V2 (xmin, py))
              L -> until (`HashMap.member` g) (+ dirv dir) (V2 (xmax, py))
              D -> until (`HashMap.member` g) (+ dirv dir) (V2 (px, ymin))
              U -> until (`HashMap.member` g) (+ dirv dir) (V2 (px, ymax))
      in
        case g HashMap.!? candidate of
          Just '.' -> candidate
          Just '#' -> pos
          Nothing  -> error "shouldn't happen"

step :: Grid -> ((N, N), (N, N)) -> (Dir, V2 N) -> Instr -> (Dir, V2 N)
step g bounds (!dir, !pos) =
  \case (Move n)  -> (dir, iterateN n (singleStep g bounds dir) pos)
        TurnLeft  -> (rotate dir CCW, pos)
        TurnRight -> (rotate dir CW, pos)

rotate :: Dir -> Rot -> Dir
rotate d r =
  case (d, r) of
    (_, RId) -> d
    (R,  CW) -> D
    (R, CCW) -> U
    (L,  CW) -> U
    (L, CCW) -> D
    (D,  CW) -> L
    (D, CCW) -> R
    (U,  CW) -> R
    (U, CCW) -> L
    (L, RHalf) -> R
    (R, RHalf) -> L
    (D, RHalf) -> U
    (U, RHalf) -> D

password :: Dir -> V2 N -> N
password dir (V2 (x, y)) =
  let ds = case dir of
             R -> 0
             D -> 1
             L -> 2
             U -> 3
  in 1000 * (y + 1) + 4 * (x + 1) + ds

part1 :: (Grid, ((N, N), (N, N)), [Instr]) -> N
part1 (g, bounds@((xmin, ymin), _), path) =
  let starting = step g bounds (R, v2 (xmin, ymin)) (Move 1)
      (dir, pos) = foldl' (step g bounds) starting path
  in password dir pos

data Dir = R
         | L
         | D
         | U
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable Dir

dirv :: Dir -> V2 N
dirv = v2 . \case R -> ( 1,  0)
                  L -> (-1,  0)
                  D -> ( 0,  1)
                  U -> ( 0, -1)

-- TODO: Remove xmin and ymin, assume 0
toCube :: Grid -> ((N, N), (N, N)) -> ([V2 N], (N, N))
toCube g ((xmin, ymin), (xmax, ymax)) =
  let xl = xmax - xmin
      yl = ymax - ymin

      cube =
        [ v2 (x, y)
        | x <- [xmin `div` 50..xmax `div` 50]
        , y <- [ymin `div` 50..ymax `div` 50]
        , v2 (x*50, y*50) `HashMap.member` g
        ]
  in (cube, (xmax `div` 50, ymax `div` 50))

universe :: (Enum a, Bounded a) => [a]
universe = [minBound..maxBound]

wrap :: (N, N) -> V2 N -> V2 N
wrap (xmax, ymax) (V2 (x, y)) =
  let xl = xmax + 1
      yl = ymax + 1
  in
    v2 ((x + xl) `mod` xl, (y + yl) `mod` yl)

links :: [V2 N] -> (N, N) -> HashMap (V2 N, Dir) (V2 N, Rot)
links cube bounds =
  HashMap.fromList
  [ ((node, dir), (pos, RId))
  | node <- cube
  , dir  <- universe
  , let pos = wrap bounds (node + dirv dir)
  , pos `elem` cube
  ]

linkStep :: [V2 N]
         -> (N, N)
         -> HashMap (V2 N, Dir) (V2 N, Rot)
         -> HashMap (V2 N, Dir) (V2 N, Rot)
linkStep cube bounds existing = HashMap.union existing . HashMap.fromList $ do
  node <- cube
  dir  <- universe
  guard $ not $ (node, dir) `HashMap.member` existing
  (t, d1, d2, rot) <- rots
  guard $ t == dir
  Just (dest1, destRot1) <- [existing HashMap.!? (node, d1) ]
  Just (dest2, destRot2) <- [existing HashMap.!? (dest1, rotate d2 destRot1)]
  pure ((node, dir), (dest2, destRot1 <> destRot2 <> rot))

data Rot = RId
         | CW
         | RHalf
         | CCW
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable Rot

instance Semigroup Rot where
  RId   <> b   = b
  a     <> RId = a
  CW    <> CCW   = RId
  CW    <> CW    = RHalf
  CW    <> RHalf = CCW
  CCW   <> CCW   = RHalf
  CCW   <> RHalf = CW
  RHalf <> RHalf = RId
  a     <> b     = b <> a

instance Monoid Rot where
  mempty = RId


rots :: [( Dir -- this is equivalent to
         , Dir -- this followed by
         , Dir -- this, with an
         , Rot -- extra twist
         )]
rots =
  [ (R, D, R, CW)
  , (L, D, L, CCW)
  , (R, U, R, CCW)
  , (L, U, L, CW)
  , (U, L, U, CCW)
  , (D, L, D, CW)
  , (U, R, U, CW)
  , (D, R, D, CCW)
  ]


-- #   -- R = CW . R . D
-- ##  -- U = CCW . U . L

--  #  -- L = CCW . L . D
-- ##  -- U = CW  . U . R

-- ##  -- D = CW  . D . L
-- #   -- R = CCW . R . U

--  #
-- ##   -- L = CW . L . U

connect :: [V2 N] -> (N, N) -> HashMap (V2 N, Dir) (V2 N, Rot)
connect cube bounds = fixpoint (linkStep cube bounds) (links cube bounds)

-- ROTATES A POINT AROUND THE CENTER OF THE FIRST QUADRANT
v2Rotate :: N -> V2 N -> Rot -> V2 N
v2Rotate m p@(V2 (x, y)) =
  \case RId   -> p
        CW    -> v2 (m - 1 - y,         x)
        CCW   -> v2 (        y, m - 1 - x)
        RHalf -> v2 (m - 1 - x, m - 1 - y)


glueJump :: HashMap (V2 N) Char
  -> ((N,N),(N,N))
  -> HashMap (V2 N, Dir) (V2 N, Rot)
  -> (N, N)
  -> Dir
  -> V2 N
  -> (Dir, V2 N)
glueJump g ((xmin, ymin), (xmax, ymax)) connections cubeBounds dir pos@(V2 (px, py)) =
  let (lx, ly) = (px `mod` 50, py `mod` 50)
      side = v2 (px `div` 50, py `div` 50)
      (tside, r) = connections HashMap.! (side, dir)
      baseTarget =
        case dir of
          R -> v2 ( 0, ly)
          L -> v2 (49, ly)
          D -> v2 (lx,  0)
          U -> v2 (lx, 49)

      rotated = v2Rotate 50 baseTarget r
      final = rotated + tside * 50
  in
    case g HashMap.!? final of
      Just '.' -> (rotate dir r, final)
      Just '#' -> (dir, pos)
      Nothing  -> error $ "should've reached somewhere here " ++ show final

singleStep' :: HashMap (V2 N) Char
  -> ((N, N), (N, N))
  -> HashMap (V2 N, Dir) (V2 N, Rot)
  -> (N, N)
  -> (Dir, V2 N)
  -> (Dir, V2 N)
singleStep' g bounds connections cubeBounds (dir, pos) =
  case g HashMap.!? (pos + dirv dir) of
    Just '.' -> (dir, pos + dirv dir)
    Just '#' -> (dir, pos)
    Nothing  -> glueJump g bounds connections cubeBounds dir pos

step' :: Grid
  -> ((N, N), (N, N))
  -> HashMap (V2 N, Dir) (V2 N, Rot)
  -> (N, N)
  -> (Dir, V2 N)
  -> Instr
  -> (Dir, V2 N)
step' g bounds connections cubeBounds (!dir, !pos) =
  let single = singleStep' g bounds connections cubeBounds
  in
    \case (Move n)  -> iterateN n single (dir, pos)
          TurnLeft  -> (rotate dir CCW, pos)
          TurnRight -> (rotate dir CW, pos)

part2 :: (Grid, ((N, N), (N, N)), [Instr]) -> N
part2 (g, bounds@((xmin, ymin), _), path) =
  let (cube, cb) = toCube g bounds
      connected = connect cube cb
      starting = step g bounds (R, v2 (xmin, ymin)) (Move 1)
      (dir, pos) = foldl' (step' g bounds connected cb) starting path
  in password dir pos

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
