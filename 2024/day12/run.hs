{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC.Grid

import Data.Bits (xor)
import Control.Monad (guard, when)
import Control.Monad.ST
import Data.Array (Array)
import Data.Array.ST
import Data.Bifunctor
import Data.Foldable hiding (find)
import Data.List hiding (find)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

------

-- Playing around with UnionFind from here:
--
-- https://byorgey.github.io/blog/posts/2024/11/02/UnionFind.html
-- https://github.com/byorgey/comprog-hs/blob/a344c1f05daa5b9f8b9c016bc0fabff207dbd777/UnionFind.hs


data UnionFind s node m = UnionFind
  { parent :: !(STArray s node node)
  , sz :: !(STUArray s node Int)
  , ann :: !(STArray s node m)
  , ufBounds :: !(node, node)
  }

new :: Ix node => (node, node) -> m -> ST s (UnionFind s node m)
new b m = newWith b (const m)

newWith :: Ix node => (node, node) -> (node -> m) -> ST s (UnionFind s node m)
newWith b m =
  UnionFind <$> newListArray b (range b)
            <*> newArray b 1
            <*> newListArray b (map m (range b))
            <*> pure b

connected :: Ix node => UnionFind s node m -> node -> node -> ST s Bool
connected uf x y = (==) <$> find uf x <*> find uf y

find :: Ix node => UnionFind s node m -> node -> ST s node
find uf@(UnionFind {..}) x = do
  p <- readArray parent x
  if p /= x
    then do
      r <- find uf p
      writeArray parent x r
      pure r
    else pure x

updateAnn :: (Ix node, Semigroup m) => UnionFind s node m -> node -> (m -> m) -> ST s ()
updateAnn uf@(UnionFind {..}) x f = do
  x <- find uf x
  old <- readArray ann x -- modifyArray is not available in Kattis test environment
  writeArray ann x (f old)

union :: (Ix node, Semigroup m) => UnionFind s node m -> node -> node -> ST s ()
union uf@(UnionFind {..}) x y = do
  x <- find uf x
  y <- find uf y
  when (x /= y) $ do
    sx <- readArray sz x
    sy <- readArray sz y
    mx <- readArray ann x
    my <- readArray ann y
    if sx < sy
      then do
        writeArray parent x y
        writeArray sz y (sx + sy)
        writeArray ann y (mx <> my)
      else do
        writeArray parent y x
        writeArray sz x (sx + sy)
        writeArray ann x (mx <> my)

size :: Ix node => UnionFind s node m -> node -> ST s Int
size uf@(UnionFind {..}) x = do
  x <- find uf x
  readArray sz x

getAnn :: Ix node => UnionFind s node m -> node -> ST s m
getAnn uf@(UnionFind {..}) x = do
  x <- find uf x
  readArray ann x

allAnns :: Ix node => UnionFind s node m -> ST s [(Int, m)]
allAnns UnionFind {..} = do
  ps <- getAssocs parent
  flip foldMap ps $ \(p, x) ->
    if p == x
      then do
        a <- readArray ann x
        s <- readArray sz x
        pure [(s, a)]
      else pure []

------

parseAll :: String -> MapGrid Char
parseAll = parseMapGrid id

data Fences = Fences { rowPieces :: !(Map (N, N) Bool)
                     , colPieces :: !(Map (N, N) Bool)
                     }
  deriving (Show, Eq)


fenceSize :: Fences -> Int
fenceSize Fences{..} = size rowPieces + size colPieces
  where size = length . filter id . toList

fenceSets :: Fences -> (Set (N, N), Set (N, N))
fenceSets Fences{..} = ( Map.keysSet (Map.filter id rowPieces)
                       , Map.keysSet (Map.filter id colPieces)
                       )

instance Semigroup Fences where
  x <> y = Fences (rowPieces x `u` rowPieces y) (colPieces x `u` colPieces y)
    where u = Map.unionWith xor

bordersOf :: (N, N) -> Fences
bordersOf (x, y) =
  Fences (Map.fromList $ map (,True) [(x, y), (x, y + 1)])
         (Map.fromList $ map (,True) [(x, y), (x + 1, y)])

fenceSegments :: Fences -> ([Int], [Int])
fenceSegments fs = runST do
  let (rowSet, colSet) = fenceSets fs
      rows = Map.fromList $ zip (toList rowSet) [0..]
      cols = Map.fromList $ zip (toList colSet) [0..]

  rowSegments <- do
    uf <- new (0, Map.size rows - 1) ()
    forM_ (Map.toList rows) \((x, y), i) -> do
      case (rows Map.!? (x + 1, y), cols Map.!? (x + 1, y))  of
        (Just j, Nothing) -> Main.union uf i j
        _      -> pure ()
    allAnns uf

  colSegments <- do
    uf <- new (0, Map.size cols - 1) ()
    forM_ (Map.toList cols) \((x, y), i) -> do
      case (cols Map.!? (x, y + 1), rows Map.!? (x, y + 1)) of
        (Just j, Nothing) -> Main.union uf i j
        _      -> pure ()
    allAnns uf

  pure ( map fst rowSegments
       , map fst colSegments
       )

fenceIt :: MapGrid Char -> [(Int, Fences)]
fenceIt g = runST do
  let (w, h) = maximum $ HashMap.keys g
      low = (0, 0)
      high = (w, h)
  uf <- newWith (low, high) bordersOf
  forM_ (HashMap.toList g) \((x, y), v) -> do
    let nbhd = do
          c <- [ (x - 1, y    )
               , (x    , y - 1)
               , (x + 1, y    )
               , (x    , y + 1)
               ]
          Just w <- pure $ g HashMap.!? c
          guard $ v == w
          pure c
    forM_ nbhd \nb ->
      Main.union uf (x, y) nb

  allAnns uf

part1 :: MapGrid Char -> Int
part1 = sum . map (uncurry (*) . second fenceSize) . fenceIt

part2 :: MapGrid Char -> Int
part2 = sum . map (uncurry (*) . second (segmentSizes . fenceSegments)) . fenceIt
  where segmentSizes (rs, cs) =
          length rs + length cs

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
