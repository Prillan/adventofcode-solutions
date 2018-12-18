{-# LANGUAGE RecordWildCards #-}
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec (ErrorItem Char) String

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

app1 :: (Int -> Int) -> Pair -> Pair
app1 f (Pair (x, y)) = Pair (f x, f y)

app2 :: (Int -> Int -> Int) -> Pair -> Pair -> Pair
app2 op (Pair (x1, y1)) (Pair (x2, y2)) = Pair (op x1 x2, op y1 y2)

newtype Pair = Pair { unPair :: (Int, Int) }
  deriving (Eq, Show, Ord)

instance Num Pair where
  (+) = app2 (+)
  (-) = app2 (-)
  (*) = app2 (*)
  abs = app1 abs
  negate = app1 negate
  signum = app1 signum
  fromInteger n = Pair (fromInteger n, fromInteger n)

type Position = Pair
type Velocity = Pair

data Light = Light { position :: Position
                   , velocity :: Velocity }
  deriving Show

data Bounds = Bounds { left, right, top, bottom :: Int }
  deriving Show

numP :: (Read a, Num a) => Parser a
numP = read <$> ((++) <$> many (char '-') <*> some digitChar)

pairP :: Parser Pair
pairP = do
  char '<'
  many spaceChar
  i <- numP
  char ','
  many spaceChar
  j <- numP
  char '>'
  pure $ Pair (i, j)

lightP :: Parser Light
lightP = do
  string "position="
  pos <- pairP
  many spaceChar
  string "velocity="
  vel <- pairP
  pure (Light pos vel)

parseAll :: String -> [Light]
parseAll =
  map unsafeRight .
  map (parse lightP "") . lines

stepLight :: Int -> Light -> Light
stepLight n (Light pos vel) = Light (pos + (fromIntegral n) * vel) vel

drawLights :: Bounds -> [Light] -> String
drawLights Bounds {..} lights =
  let positions = Set.fromList . map position $ lights
  in
    unlines $ [[ if Set.member (Pair (c, r)) positions then '#' else '.'
               | c <- [left..right]]
               | r <- [top..bottom]]

boundary :: [Light] -> Bounds
boundary lights =
  let pos = map (unPair . position) lights
  in Bounds { top    = minimum . map snd $ pos
            , bottom = maximum . map snd $ pos
            , right  = maximum . map fst $ pos
            , left   = minimum . map fst $ pos }

area :: Bounds -> Int
area Bounds {..} = (top - bottom) * (left - right)

stepUntilArea :: Int -> Int -> [Light] -> (Int, [Light])
stepUntilArea a stepSize =
  head
  . filter ((< a) . area . boundary . snd)
  . zip [0,stepSize..]
  . iterate (map (stepLight stepSize))

solve :: [Light] -> (Int, [Light])
solve input =
  let (steps, close) = stepUntilArea 50000 10 input
  in minimumBy (comparing (area . boundary . snd))
     . take 10
     . filter ((< 10000) . area . boundary . snd)
     . zip [steps..]
     . iterate (map (stepLight 1)) $ close

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   let (steps, lights) = solve input
   putStrLn $ "Step #" ++ show steps
   putStrLn $ drawLights (boundary lights) lights
