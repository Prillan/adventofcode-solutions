input = ".^.^..^......^^^^^...^^^...^...^....^^.^...^.^^^^....^...^^.^^^...^^^^.^^.^.^^..^.^^^..^^^^^^.^^^..^"

step prev = zipWith3 z ("." ++ prev) prev (drop 1 prev ++ ".")
  where z x y z
         | x == '^' && y == '^' && z /= '^' = '^'
         | x /= '^' && y == '^' && z == '^' = '^'
         | x == '^' && y /= '^' && z /= '^' = '^'
         | x /= '^' && y /= '^' && z == '^' = '^'
         | otherwise = '.'

part1 = length . filter (== '.') . concat . take 40 . iterate step
part2 = length . filter (== '.') . concat . take 400000 . iterate step

main = do
   print (part1 input)
   print (part2 input)
