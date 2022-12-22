{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
import AoC
import AoC.Grid

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

type Parser a = Parsec Void String a
type N = Int

filterGrid = HashMap.filter (`elem` ".#")

data Instr = Move N
           | TurnLeft
           | TurnRight
  deriving (Show, Eq)

numP :: Num a => Parser a
numP = fromInteger . read <$> some digitChar

pathP = many instrP

instrP =
  choice
  [ Move <$> numP
  , TurnLeft <$ char 'L'
  , TurnRight <$ char 'R'
  ]

parseAll :: String -> (HashMap (V2 N) Char, ((N,N),(N,N)), [Instr])
parseAll input =
  let [grid, path] = splitOn "\n\n" input
      g = filterGrid $ parseMapGrid id grid
      (xs, ys) = unzip $ HashMap.keys g
      g' = HashMap.mapKeys v2 g
      bounds = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  in (g', bounds, (\(Right x) -> x) $ parse pathP "" path)

singleStep g ((xmin, ymin), (xmax, ymax)) dir pos@(V2 (px, py)) =
  case g HashMap.!? (pos + dirv dir) of
    Just '.' -> pos + dirv dir
    Just '#' -> pos
    Nothing  ->
      let candidate =
            case dir of
              R -> until ((`HashMap.member` g) . tce "r") (+ dirv dir) (V2 (xmin, py))
              L -> until ((`HashMap.member` g) . tce "l") (+ dirv dir) (V2 (xmax, py))
              D -> until ((`HashMap.member` g) . tce "d") (+ dirv dir) (V2 (px, ymin))
              U -> until ((`HashMap.member` g) . tce "u") (+ dirv dir) (V2 (px, ymax))
      in
        case g HashMap.!? candidate of
          Just '.' -> candidate
          Just '#' -> pos
          Nothing  -> error "shouldn't happen"

step g bounds (!dir, !pos) =
  \case (Move n)  -> tce ("moved " <> show n) (dir, iterateN n (singleStep g bounds dir) pos)
        TurnLeft  -> tce "turned left" (rotate dir CCW, pos)
        TurnRight -> tce "turned right" (rotate dir CW, pos)

rotate :: Dir -> Rot -> Dir
rotate d r =
  case (d, r) of
    (R,  CW) -> D
    (R, CCW) -> U
    (L,  CW) -> U
    (L, CCW) -> D
    (D,  CW) -> L
    (D, CCW) -> R
    (U,  CW) -> R
    (U, CCW) -> L

password dir (V2 (x, y)) =
  let ds = case dir of
             R -> 0
             D -> 1
             L -> 2
             U -> 3
  in 1000 * (y + 1) + 4 * (x + 1) + ds

part1 (g, bounds@((xmin, ymin), _), path) =
  let starting = step g bounds (R, v2 (xmin, ymin)) (Move 1)
      (dir, pos) = tce "final" $ foldl' (step g bounds) starting path
  in password dir pos

data Dir = R
         | L
         | D
         | U
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Hashable Dir

dirv = v2 . \case R -> ( 1,  0)
                  L -> (-1,  0)
                  D -> ( 0,  1)
                  U -> ( 0, -1)
                    
-- TODO: Remove xmin and ymin, assume 0
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

universe = [minBound..maxBound]

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

linkStep :: [V2 N] -> (N, N) -> HashMap (V2 N, Dir) (V2 N, Rot) -> HashMap (V2 N, Dir) (V2 N, Rot)
linkStep cube bounds existing = HashMap.union existing . HashMap.fromList $ do
  node <- cube
  dir  <- universe
  guard $ not $ (node, dir) `HashMap.member` existing
  let pos = wrap bounds (node + dirv dir)
  (t, d1, d2, rot) <- rots
  guard $ t == dir
  Just (dest1, destRot1) <- [existing HashMap.!? (node, d1) ]
  Just (dest2, destRot2) <- [existing HashMap.!? (dest1, d2)]
  pure $ ((node, dir), (dest2, destRot1 <> destRot2 <> rot))
  

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

-- ##
-- #   -- R = CCW . R . U

--  #
-- ##   -- L = CW . L . U


--  #
--  #
-- ###
--  #

--  ##
--  #
-- ##
-- #
connect cube bounds = fixpoint (linkStep cube bounds) (links cube bounds)


singleStep' g ((xmin, ymin), (xmax, ymax)) dir pos@(V2 (px, py)) =
  case g HashMap.!? (pos + dirv dir) of
    Just '.' -> pos + dirv dir
    Just '#' -> pos
    Nothing  ->
      let candidate =
            case dir of
              R -> until ((`HashMap.member` g) . tce "r") (+ dirv dir) (V2 (xmin, py))
              L -> until ((`HashMap.member` g) . tce "l") (+ dirv dir) (V2 (xmax, py))
              D -> until ((`HashMap.member` g) . tce "d") (+ dirv dir) (V2 (px, ymin))
              U -> until ((`HashMap.member` g) . tce "u") (+ dirv dir) (V2 (px, ymax))
      in
        case g HashMap.!? candidate of
          Just '.' -> candidate
          Just '#' -> pos
          Nothing  -> error "shouldn't happen"

step' g bounds (!dir, !pos) =
  \case (Move n)  -> tce ("moved " <> show n) (dir, iterateN n (singleStep' g bounds dir) pos)
        TurnLeft  -> tce "turned left" (rotate dir CCW, pos)
        TurnRight -> tce "turned right" (rotate dir CW, pos)

part2 (g, bounds@((xmin, ymin), _), path) = ()
  -- let (cube, cb) = toCube g bounds
  --     starting = step g bounds (R, v2 (xmin, ymin)) (Move 1)
  --     (dir, pos) = tce "final" $ foldl' (step' g bounds) starting path
  -- in password dir pos

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
