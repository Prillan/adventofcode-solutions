import Text.Megaparsec

type Parser = Parsec Dec String

right :: (Show a) => Either a b -> b
right (Left x) = error $ show x
right (Right x) = x

data DataStream = Group [DataStream]
                | Garbage Int
  deriving (Show, Eq, Ord)

groupP :: Parser DataStream
groupP = do
  char '{'
  inner <- sepBy streamP (char ',')
  char '}'
  pure $ Group inner

garbageP :: Parser DataStream
garbageP = do
  char '<'
  count <- eat
  pure $ Garbage count

eat :: Parser Int
eat = eat' 0

eat' :: Int -> Parser Int
eat' i = do
  c <- anyChar
  case c of
    '>' -> pure i
    '!' -> anyChar *> eat' i
    _   -> eat' (i + 1)

streamP :: Parser DataStream
streamP = groupP <|> garbageP

parser :: Parser DataStream
parser = streamP <* eof

part1 = go 1
  where go _ (Garbage _) = 0
        go i (Group gs) = i + sum (map (go (i+1)) gs)

part2 = go
  where go (Garbage c) = c
        go (Group gs) = sum (map go gs)

main = do
  input <- right . parse parser "" . head . lines <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
