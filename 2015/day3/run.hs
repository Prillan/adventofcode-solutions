import qualified Data.Set as Set

process = length . snd . foldl f ((0, 0), Set.insert (0,0) Set.empty)
  where f (cur, visited) dir = (next, Set.insert next visited)
          where next = move cur dir
        move (x, y) dir | dir == '^' = (x, y+1)
                        | dir == '>' = (x+1, y)
                        | dir == 'v' = (x, y-1)
                        | dir == '<' = (x-1, y)

main = do
   input <- readFile "input.txt"
   print (process input)
