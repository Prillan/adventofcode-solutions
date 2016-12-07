import           Data.List (isInfixOf)
import           Text.Megaparsec

unsafeRight :: Either a b -> b
unsafeRight (Right x) = x

data Segment = Normal String | Hypernet String
  deriving (Show, Eq)
newtype IPv7 = IPv7 { toSegments :: [Segment] }
  deriving (Show, Eq)

hypernets :: IPv7 -> [String]
hypernets ip = do
  Hypernet v <- toSegments ip
  pure v

normals :: IPv7 -> [String]
normals ip = do
  Normal v <- toSegments ip
  pure v

parseIPv7 :: Parsec Dec String IPv7
parseIPv7 = IPv7 <$> some (hypernet <|> normal)
  where str = some letterChar
        normal = Normal <$> str
        hypernet = Hypernet <$> between (char '[') (char ']') str

parseAll :: String -> [IPv7]
parseAll = map unsafeRight .
  map (parse parseIPv7 "") . lines


hasAbba :: String -> Bool
hasAbba (x:y:z:w:rest)
  | x /= y && x == w && y == z = True
  | y /= z && z == w           = hasAbba (y:z:w:rest)
  | z /= w                     = hasAbba (z:w:rest)
  | otherwise                  = hasAbba rest
hasAbba _ = False

supportsTLS :: IPv7 -> Bool
supportsTLS ip =  (any hasAbba . normals $ ip)
               && (all (not . hasAbba) . hypernets $ ip)

type ABA = (Char, Char, Char)

allABAs :: [String] -> [ABA]
allABAs =
  filter (\(x, y, z) -> x == z && x /= y)
  . concat
  . map (\s -> zip3 s (drop 1 s) (drop 2 s))

supportsSSL :: IPv7 -> Bool
supportsSSL ip = any (\aba -> any (hasBAB aba) (hypernets ip)) abas
  where abas = allABAs $ normals ip
        hasBAB (a, b, _) = isInfixOf (b:a:b:[])

part1 :: [IPv7] -> Int
part1 = length . filter supportsTLS
part2 :: [IPv7] -> Int
part2 = length . filter supportsSSL

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
