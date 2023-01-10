{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid
import AoC.Parse (numP)

import Data.Foldable
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type N = Int

type Parser = Parsec Void String

type Bounds = (N, N)

data Instruction = Instr Bool Bounds Bounds Bounds
  deriving Show

isOn :: Instruction -> Bool
isOn = \case Instr on _ _ _ -> on

instrP :: Parser Instruction
instrP = do
  b <- (== "on") <$> (try (string "on") <|> string "off")
  _ <- char ' '
  x <- boundsP 'x'
  _ <- char ','
  y <- boundsP 'y'
  _ <- char ','
  z <- boundsP 'z'
  pure $ Instr b x y z

boundsP :: Char -> Parser Bounds
boundsP c = do
  _ <- char c *> char '='
  l <- numP
  _ <- string ".."
  h <- numP
  pure (l, h)

parseAll :: String -> [Instruction]
parseAll =
  map (\case Right v -> v)
  . map (\s -> parse instrP "" s)
  . lines

data Cuboid = Cuboid { cX :: (N, N)
                     , cY :: (N, N)
                     , cZ :: (N, N)
                     }
  deriving (Show, Eq, Ord)

-- EXCLUSIVE upper bounds
volume :: Cuboid -> Integer
volume Cuboid {..} = s cX * s cY * s cZ
  where s (l, h) = fromIntegral $ h - l

cuboid :: Instruction -> Cuboid
cuboid (Instr _ x y z) =
  Cuboid x y z

type SplitCubes = ([Bounds], [Bounds], [Bounds])

-- inclusive bounds
overlaps1d :: Bounds -> Bounds -> Bool
overlaps1d (l1, h1) (l2, h2) =
  not $ h1 < l2 || l1 > h2

-- inclusive bounds
overlaps3d :: Cuboid -> Cuboid -> Bool
overlaps3d c1 c2 =
  overlaps1d (cX c1) (cX c2)
  && overlaps1d (cY c1) (cY c2)
  && overlaps1d (cZ c1) (cZ c2)

clusters :: [Cuboid] -> [[Cuboid]]
clusters = foldr merge []
  where merge c as =
          let (matched, rest) = partition (any (overlaps3d c)) as
          in (c:concat matched):rest

instructionClusters :: [Instruction] -> [[Instruction]]
instructionClusters instrs =
  let cs = clusters $ map cuboid instrs
  in
    map (\c -> filter (\i -> cuboid i `elem` c) instrs) cs

-- exclusive upper bound
overlaps1d' :: Bounds -> Bounds -> Bool
overlaps1d' (l1, h1) (l2, h2) =
  not $ h1 <= l2 || l1 >= h2

-- exclusive upper bounds
overlaps3d' :: Cuboid -> Cuboid -> Bool
overlaps3d' c1 c2 =
  overlaps1d' (cX c1) (cX c2)
  && overlaps1d' (cY c1) (cY c2)
  && overlaps1d' (cZ c1) (cZ c2)

cuboid' :: Instruction -> Cuboid
cuboid' (Instr _ (xl, xh) (yl, yh) (zl, zh)) =
  Cuboid (xl, xh+1) (yl, yh+1) (zl, zh+1)

fullCovering' :: [Cuboid] -> SplitCubes
fullCovering' cuboids =
  let bx = bounds' $ map cX cuboids
      by = bounds' $ map cY cuboids
      bz = bounds' $ map cZ cuboids
  in (bx, by, bz)

bounds' :: [Bounds] -> [Bounds]
bounds' bs =
  let combined = IntSet.toList
                 . IntSet.fromList
                 . concatMap (\(l, h) -> [l, h])
                 $ bs
  in zip combined (drop 1 combined)

covering' :: Bounds -> [Bounds] -> [Bounds]
covering' (l, h) = takeWhile p . dropWhile (not . p)
  where p (l', _) = l <= l' && l' < h

covering3d' :: SplitCubes -> Cuboid -> [Cuboid]
covering3d'  (bsx, bsy, bsz) c =
  let bsx' = covering' (cX c) bsx
      bsy' = covering' (cY c) bsy
      bsz' = covering' (cZ c) bsz
  in
    Cuboid <$> bsx' <*> bsy' <*> bsz'

-- split two lists of cuboids so that they overlap exactly
jitSplit :: [Cuboid] -> [Cuboid] -> ([Cuboid], [Cuboid])
jitSplit xs ys = go xs ys []
  where go xs [] acc = (xs, acc)
        go xs (y:ys) acc =
          case partition (overlaps3d' y) xs of
            ([], _) -> go xs ys (y:acc)
            (ol, rest) ->
              let cs  = fullCovering' (y:ol)
                  xs' = concatMap (covering3d' cs) ol
                  ys' = concatMap (covering3d' cs) ys
                  y'  = covering3d' cs y
              in go (xs' ++ rest) ys' (y' ++ acc)

-- TODO: improvement, specialize the split above to perform the
-- operation too.

partSolve' :: [Instruction] -> Integer
partSolve' = sum . map volume . toList . foldl' f Set.empty
  where f on i =
          let (on', i') = jitSplit (toList on) [cuboid' i]
          in if isOn i
             then Set.fromList $ on' ++ i'
             else Set.fromList on' `Set.difference` Set.fromList i'

part1 :: [Instruction] -> Integer
part1 = partSolve' . head . instructionClusters

part2 :: [Instruction] -> Integer
part2 = sum . map partSolve' . instructionClusters

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
