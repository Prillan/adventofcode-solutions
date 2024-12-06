{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Foldable (toList)
import Data.Maybe (isJust)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Set (Set)
import qualified Data.Set as Set

type N = Int

type Input = MapGrid Char

parseAll :: String -> Input
parseAll = parseMapGrid id

-- Note: flipped, because we have (0, 0) at the top left corner
turnRight :: V2 N -> V2 N
turnRight (V2 (x, y)) = v2 (-y, x)

-- TODO: Extract to AoC.Grid
iray :: MapGrid Char -> V2 N -> (N, N) -> [((N, N), Char)]
iray g d pos =
  concatMap toList
  . takeWhile isJust
  . map (\(V2 p) -> (p,) <$> g HashMap.!? p)
  $ map (\i -> fromInteger i * d + v2 pos) [0..]

startPos :: MapGrid Char -> (N, N)
startPos = fst . head . filter ((== '^') . snd) . HashMap.toList

walk :: Monoid m => ([(N, N)] -> m) -> Input -> (m, Bool)
walk f g = go (startPos g) startDir mempty Set.empty
  where go !p !d visited loop
         | (p, d) `Set.member` loop = (visited, True)
         | otherwise =
           let r = iray g' d p
           in
             case break ((== '#') . snd) r of
               (v, [])    -> (visited <> f (map fst v), False)
               (v,  _) -> let poss = map fst v
                              p'   = last poss
                              d'   = turnRight d
                          in go p' d' (visited <> f poss) (Set.insert (p, d) loop)
        startDir = v2 (0, -1)
        g' = HashMap.insert (startPos g) '.' g

part1 :: Input -> Int
part1 = length . fst . walk Set.fromList

part2 :: Input -> Int
part2 g =
  let (candidates, False) = walk Set.fromList g
  in
    length
    . filter (snd . walk (const ()))
    . map (\c -> HashMap.insert c '#' g)
    . filter (/= startPos g)
    $ Set.toList candidates

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
