{-# LANGUAGE LambdaCase #-}

parse :: String -> (Char, Char)
parse =
  \case [a, _, x] ->
          (a, x)

parseAll :: String -> [(Char, Char)]
parseAll = map parse  . lines

score1 :: (Char, Char) -> Int
score1 round =
  let result = case round of
                 ('A', 'X') -> 3
                 ('A', 'Y') -> 6
                 ('A', 'Z') -> 0
                 ('B', 'X') -> 0
                 ('B', 'Y') -> 3
                 ('B', 'Z') -> 6
                 ('C', 'X') -> 6
                 ('C', 'Y') -> 0
                 ('C', 'Z') -> 3
      choice = case snd round of
                 'X' -> 1
                 'Y' -> 2
                 'Z' -> 3
  in result + choice

score2 :: (Char, Char) -> Int
score2 round =
  let (result, chosen) =
        case round of
          ('A', 'X') -> (0, 'C')
          ('A', 'Y') -> (3, 'A')
          ('A', 'Z') -> (6, 'B')
          ('B', 'X') -> (0, 'A')
          ('B', 'Y') -> (3, 'B')
          ('B', 'Z') -> (6, 'C')
          ('C', 'X') -> (0, 'B')
          ('C', 'Y') -> (3, 'C')
          ('C', 'Z') -> (6, 'A')
      choice = case chosen of
                 'A' -> 1
                 'B' -> 2
                 'C' -> 3
  in result + choice

part1 :: [(Char, Char)] -> Int
part1 = sum . map score1

part2 :: [(Char, Char)] -> Int
part2 = sum . map score2

main :: IO ()
main = main' "input.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
