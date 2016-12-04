import Data.List (group)

process :: String -> [Int]
process = map length . iterate step
  where step = concatMap (\g -> (show $ length g) ++ take 1 g) . group

main = do
   let input = "1113122113"
   putStrLn ("part1: " ++ show (process input !! 40))
   putStrLn ("part2: " ++ show (process input !! 50))
