import AoC.Grid
import AoC.Search

import Data.Char (ord)
import Data.List (find)
import Data.Maybe (fromJust, mapMaybe)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

parseAll :: String -> MapGrid Char
parseAll = parseMapGrid id

-- TODO: Extract as helpers to AoC.Grid
type Pos = (Int, Int)
hv :: Pos -> [Pos]
hv (i, j) = [ (i + dx, j + dy) | dx <- [-1..1]
                               , dy <- [-1..1]
                               , dx == 0 || dy == 0 ]

hvNeighbors :: HashMap Pos a -> Pos -> [(Pos, a)]
hvNeighbors m =
  mapMaybe (\pos -> sequence (pos, pos `HashMap.lookup` m))
  . hv

validElev :: Int -> Int -> Bool
validElev from to =
  to <= from + 1

part1 :: MapGrid Char -> Int
part1 g =
  let Just (start, _) = find ((== 'S') . snd) . HashMap.toList $ g
      Just (end,   _) = find ((== 'E') . snd) . HashMap.toList $ g
      g' = HashMap.map ord
           . HashMap.insert start 'a'
           . HashMap.insert end 'z'
           $ g

      neighbors (pos, elev) = filter (validElev elev . snd) $ hvNeighbors g' pos
  in fromJust $ bfs_ ((== end) . fst) neighbors (start, ord 'a')

part2 :: MapGrid Char -> Int
part2 g = 
  let Just (start, _) = find ((== 'S') . snd) . HashMap.toList $ g
      Just (end,   _) = find ((== 'E') . snd) . HashMap.toList $ g
      g' = HashMap.map ord
           . HashMap.insert start 'a'
           . HashMap.insert end 'z'
           $ g
      neighbors (pos, elev) = filter (flip validElev elev . snd) $ hvNeighbors g' pos
  in
    fromJust $ bfs_ ((== ord 'a') . snd) neighbors (end, ord 'z')


main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
