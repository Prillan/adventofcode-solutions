{-# LANGUAGE BangPatterns #-}
import           Data.List (permutations, group, minimum, maximum, minimumBy, maximumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as VM
import           Data.Void (Void)
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char

type Reg = Char
type Val = Int
type ValReg = Either Val Reg
data Instr = Cpy ValReg ValReg
           | Jnz ValReg ValReg
           | Inc ValReg
           | Dec ValReg
           | Tgl ValReg
           | Stop
  deriving (Show, Eq)

numP = read <$> ((++) <$> many (char '-') <*> some digitChar)
regP = oneOf "abcd"

valRegP = fmap Left numP <|> fmap Right regP

cpyP =
  Cpy <$> (string "cpy " *> valRegP)
      <*> (string " " *> valRegP)

jnzP =
  Jnz <$> (string "jnz " *> valRegP)
      <*> (string " " *> valRegP)

incP =
  Inc <$> (string "inc " *> valRegP)

decP =
  Dec <$> (string "dec " *> valRegP)

tglP =
  Tgl <$> (string "tgl " *> valRegP)

instrP :: Parsec Void String Instr
instrP = cpyP <|> jnzP <|> incP <|> decP <|> tglP


data Zipper a = Z [a] a [a]
  deriving Show

instance Functor Zipper where
  fmap f (Z l c r) = Z (f <$> l) (f c) (f <$> r)

-- current (Z _ c _) = c

left (Z [] _ _) = undefined
left (Z (l:ls) c rs) = Z ls l (c:rs)

right (Z _ _ []) = undefined
right (Z ls c (r:rs)) = Z (c:ls) r rs

end (Z _ _ []) = True
end (Z _ Stop _) = True
end _ = False

-- fromList :: a -> [a] -> Zipper a
-- fromList def xs =
--   let (x':xs') = xs ++ repeat def
--   in Z (repeat def) x' xs'

type Regs = Map Reg Val
type State = (Regs, Int, Vector Instr)

newState instr = ( Map.fromList (zip "abcd" (repeat 0))
                 , 0
                 , V.fromList instr)

val :: State -> Either Val Reg -> Val
val _ (Left v) = v
val (m, _, _) (Right r) = m Map.! r

iter :: (a -> a) -> Int -> a -> a
iter f n = foldr (.) id (replicate n f)

forward :: State -> Val -> State
forward (r, !p, i) n = (r, p + n, i)

backward :: State -> Val -> State
backward (r, !p, i) n = (r, p - n, i)

jnz :: State -> Val -> Val -> State
jnz s v steps
  | v == 0               = s
  | v /= 0 && steps >= 0 = forward s steps
  | v /= 0 && steps <  0 = backward s (negate steps)

cpy :: State -> Val -> Reg -> State
cpy (!m, p, i) v k  = (Map.insert k v m, p, i)

inc :: State -> Reg -> State
inc (!m, p, i) k = (Map.update (pure . (+1)) k m, p, i)

dec :: State -> Reg -> State
dec (!m, p, i) k = (Map.update (pure . (+ (-1))) k m, p, i)

tgl :: State -> Val -> State
tgl s v
  | abs v >= 30 = s
  | otherwise =
    let (d1, d2) =
          if v >= 0
            then (forward, backward)
            else (backward, forward)
        (m, p, i) = d1 s (abs v)
        !i' = case i V.!? p of
                Just instr -> V.modify (\v -> VM.write v p (tgl' instr)) i
                _          -> i
    in
      d2 (m, p, i') (abs v)

tgl' :: Instr -> Instr
tgl' Stop = Stop
tgl' (Jnz v o) = Cpy v o
tgl' (Cpy v t) = Jnz v t
tgl' (Inc r) = Dec r
tgl' (Dec r) = Inc r
tgl' (Tgl v) = Inc v

current (_, p, i) = maybe Stop id (i V.!? p)

exec1 :: State -> State
exec1 s =
  let !s' = case current s of
              Jnz v steps -> jnz s (val s v) ((val s steps)-1)
              Cpy v (Right r) -> cpy s (val s v) r
              Inc (Right r) -> inc s r
              Dec (Right r) -> dec s r
              Tgl v -> tgl s (val s v)
              Stop -> backward s 1
              _ -> s
  in
    forward s' 1

exec :: State -> State
exec = head . dropWhile ((/= Stop) . current) . iterate exec1

unsafeRight (Right x) = x

parseAll = map unsafeRight .
  map (parse instrP "") . lines

run input x =
  let (m, p, i) = newState input
      (regs, _, _) = exec (Map.insert 'a' x m , p, i)
  in regs Map.! 'a'

part1 input = run input 7
part2 input = run input 12

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
