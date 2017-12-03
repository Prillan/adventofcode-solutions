part1Input :: Int
part1Input = 289326

part1 :: Int -> Int
part1 input
  | input == 1 = 0
  | otherwise =
    let l = layer input
        ls = layerSideSize l
        pos = input - (1 + layerExit (l - 1))
    in
      (l - 1) + (abs $ offset pos l)

layerSideSize :: Int -> Int
layerSideSize l = (l * 2) - 1

layerSize :: Int -> Int
layerSize l
  | l == 1    = 1
  | otherwise = (l - 1) * 8

layers :: [Int]
layers = map layerSize [1..]

layerExit :: Int -> Int
layerExit l = ((l * 2) - 1) ^ 2

sizeBelow = scanl (+) 0 layers

layer :: Int -> Int
layer i
  | i == 1    = 1
  | otherwise = head $ filter (\l -> i <= layerExit l) [1..]

shift :: Int -> Int
shift l
  | l == 1    = 0
  | otherwise = l - 2

offset :: Int -> Int -> Int
offset i l =
  let i' = i `mod` (layerSideSize l - 1)
  in
    i' - (shift l)


--- PART 2

locations :: [(Int, Int)]
locations = map fst3 $ iterate f ((0, 0), 1, 1)
  where fst3 (x0, _, _) = x0
        f ((x, y), l, i)
          | i >= (layerSideSize l - 1) * 4 = ((x + 1, y), l + 1, 1)
          | i >= (layerSideSize l - 1) * 3 = ((x + 1, y), l, i + 1)
          | i >= (layerSideSize l - 1) * 2 = ((x, y - 1), l, i + 1)
          | i >= (layerSideSize l - 1) * 1 = ((x - 1, y), l, i + 1)
          | otherwise                      = ((x, y + 1), l, i + 1)

location :: Int -> (Int, Int)
location 1 = (0, 0)
location i = locations !! (i - 1)

indices :: [((Int, Int), Int)]
indices = zip locations [1..]

index :: (Int, Int) -> Int
index pos = snd . head . filter (\(p, _) -> p == pos) $ indices

memory :: [Int]
memory = map valueIn locations
   where valueIn (0, 0) = 1
         valueIn (1, 0) = 1
         valueIn pos = sum $ map valueIn $ adjacent pos

         adjacent (posx, posy) =
           let potential =
                 [(posx + x, posy + y) | x <- [-1..1], y <- [-1..1]]
               i = index (posx, posy)
           in
             filter (\pos -> index pos < i) potential

part2Input :: Int
part2Input = part1Input

part2 :: Int -> Int
part2 input = head . dropWhile (< input) $ memory

main :: IO ()
main = do
   print (part1 part1Input)
   print (part2 part2Input)
