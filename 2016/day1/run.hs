import           Control.Monad (foldM)
import           Data.Set (Set)
import qualified Data.Set as Set

parseAll =
  map (read :: String -> Instr) . words

data Instr = R Integer | L Integer
  deriving Show

instance Read Instr where
  readsPrec _ (d:rest) =
    case d of
      'R' -> map (\(v, _) -> (R v, "")) (reads rest)
      'L' -> map (\(v, _) -> (L v, "")) (reads rest)
      _   -> mempty
  readsPrec _ _ = mempty

infixr 6 :+
data Complex = Integer :+ Integer
  deriving (Eq, Show, Ord)
real (a :+ _) = a
img  (_ :+ b) = b

instance Num Complex where
  negate (a :+ b) = (negate a) :+ (negate b)
  (a:+b) + (c:+d) = (a+c) :+ (b+d)
  (a:+b) * (c:+d) = (a*c - b*d) :+ (a*d + b*c)
  fromInteger x = x :+ 0
  abs (a :+ b) = fromInteger $ a + b

i :: Complex
i = 0 :+ 1

dirMod :: Instr -> Complex
dirMod (R _) = negate i
dirMod (L _) = i

instrLen (R x) = x :+ 0
instrLen (L x) = x :+ 0

startPos :: Complex
startPos = 0 :+ 0
startDir :: Complex
startDir = 0 :+ 1

part1 = real . abs . fst . foldl step (startPos, startDir)
  where step (pos, dir) instr =
          let newDir = dir * dirMod instr
              newPos = pos + (instrLen instr * newDir)
          in
            (newPos, newDir)
thd4 (_, _, x, _) = x
fth4 (_, _, _, x) = x

range (a :+ b) (c :+ d)
  | a == c = (a :+) <$> enumFromThenTo b (b + signum (d - b)) d
  | b == d = (:+ b) <$> enumFromThenTo a (a + signum (c - a)) c

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = mb . foldM step Set.empty
  where step s n = case n `elem` s of
          True  -> Left n
          False -> pure $ Set.insert n s
        mb (Left v) = Just v
        mb _        = Nothing

part2 input = real . abs <$> firstDuplicate overlappingPath
  where overlappingPath = thd4 . head .
          filter fth4 . scanl step (startPos, startDir, [startPos], False) $ input

        step (pos, dir, visited, visitedBefore) instr =
          let newDir = dir * dirMod instr
              newPos = pos + (instrLen instr * newDir)
              path = drop 1 $ range pos newPos
          in
            (newPos, newDir, visited ++ path, any (`elem` visited) path)


main = do
  input <- parseAll <$> readFile "input.txt"
  print (part1 input)
  mapM_ print (part2 input)
