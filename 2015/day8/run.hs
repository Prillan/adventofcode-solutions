{-# LANGUAGE LambdaCase #-}

readString :: String -> String
readString = go . tail . init
  where go :: String -> String
        go = \case
          []                  -> []
          '\\':'x':c1:c2:rest -> hexToChar c1 c2:go rest
          '\\':c:rest         -> c:go rest
          c:rest              -> c:go rest

hexToChar :: Char -> Char -> Char
hexToChar c1 c2 = read $ "'\\x" ++ [c1, c2] ++ "'"


part1 input =
  let sl = length $ concat input
      ml = length $ concatMap readString input
  in sl - ml


part2 input =
  let sl = length $ concat input
      el = length $ concatMap show input
  in el - sl

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
  input <- lines <$> readFile file
  print (part1 input)
  print (part2 input)
