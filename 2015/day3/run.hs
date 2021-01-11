import qualified Data.Set as Set

part1 = length . snd . foldl f ((0, 0), Set.insert (0,0) Set.empty)
  where f (cur, visited) dir = (next, Set.insert next visited)
          where next = move cur dir
        move (x, y) dir | dir == '^' = (x, y+1)
                        | dir == '>' = (x+1, y)
                        | dir == 'v' = (x, y-1)
                        | dir == '<' = (x-1, y)

santa = snd . foldl f ((0, 0), Set.insert (0,0) Set.empty)
  where f (cur, visited) dir = (next, Set.insert next visited)
          where next = move cur dir
        move (x, y) dir | dir == '^' = (x, y+1)
                        | dir == '>' = (x+1, y)
                        | dir == 'v' = (x, y-1)
                        | dir == '<' = (x-1, y)

fork xs = (half True, half False)
  where half b = map snd . filter ((b==) . fst) $ zip (cycle [True, False]) xs

part2 inst = length $ Set.union (santa realSanta) (santa roboSanta)
  where (realSanta, roboSanta) = fork inst

main = do
   input <- readFile "input.txt"
   print (part1 input)
   print (part2 input)
