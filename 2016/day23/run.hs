{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
import           Data.Functor ((<&>))
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

data Reg = A | B | C | D
  deriving (Show, Eq)
type Val = Int
type ValReg = Either Val Reg
data Instr = Cpy ValReg ValReg
           | Jnz ValReg ValReg
           | Inc ValReg
           | Dec ValReg
           | Tgl ValReg
           | Stop
  deriving (Show, Eq)

type Parser a = Parsec Void String a

numP :: Parser Int
numP = read <$> ((++) <$> many (char '-') <*> some digitChar)

regP :: Parser Reg
regP = oneOf "abcd" <&> \case
  'a' -> A
  'b' -> B
  'c' -> C
  'd' -> D

valRegP :: Parser ValReg
valRegP = Left <$> numP <|> Right <$> regP

cpyP :: Parser Instr
cpyP =
  Cpy <$> (string "cpy " *> valRegP)
      <*> (string " " *> valRegP)

jnzP :: Parser Instr
jnzP =
  Jnz <$> (string "jnz " *> valRegP)
      <*> (string " " *> valRegP)

incP :: Parser Instr
incP =
  Inc <$> (string "inc " *> valRegP)

decP :: Parser Instr
decP =
  Dec <$> (string "dec " *> valRegP)

tglP :: Parser Instr
tglP =
  Tgl <$> (string "tgl " *> valRegP)

instrP :: Parsec Void String Instr
instrP = cpyP <|> jnzP <|> incP <|> decP <|> tglP

data Regs = Regs { regA, regB, regC, regD :: !Val }
type State = (Regs, Int, Vector Instr)

newState :: [Instr] -> State
newState instr = ( emptyRegs
                 , 0
                 , V.fromList instr)

emptyRegs :: Regs
emptyRegs = Regs 0 0 0 0

regVal :: Regs -> Reg -> Val
regVal Regs {..} =
  \case
    A -> regA
    B -> regB
    C -> regC
    D -> regD

setReg :: Reg -> Val -> Regs -> Regs
setReg r v regs =
  case r of
    A -> regs { regA = v }
    B -> regs { regB = v }
    C -> regs { regC = v }
    D -> regs { regD = v }

modifyReg :: (Val -> Val) -> Reg -> Regs -> Regs
modifyReg f r regs@Regs {..} =
  case r of
    A -> regs { regA = f regA }
    B -> regs { regB = f regB }
    C -> regs { regC = f regC }
    D -> regs { regD = f regD }

val :: State -> Either Val Reg -> Val
val _ (Left v) = v
val (m, _, _) (Right r) = regVal m r

forward :: State -> Val -> State
forward (r, !p, i) n = (r, p + n, i)

backward :: State -> Val -> State
backward (r, !p, i) n = (r, p - n, i)

jnz :: State -> Val -> Val -> State
jnz s v steps
  | v == 0     = s
  | steps >= 0 = forward s steps
  | steps <  0 = backward s (negate steps)

cpy :: State -> Val -> Reg -> State
cpy (m, p, i) v k  = (setReg k v m, p, i)

inc :: State -> Reg -> State
inc (m, p, i) k = (modifyReg (\v -> v + 1) k m, p, i)

dec :: State -> Reg -> State
dec (m, p, i) k = (modifyReg (\v -> v - 1) k m, p, i)

tgl :: State -> Val -> State
tgl s@(m, p, i) n
  | abs n >= 30 = s
  | otherwise =
    let !i' = case i V.!? (p + n) of
                Just instr -> V.modify (\v -> VM.write v (p + n) (tgl' instr)) i
                _          -> i
    in (m, p, i')

tgl' :: Instr -> Instr
tgl' = \case Jnz v o -> Cpy v o
             Cpy v t -> Jnz v t
             Inc r -> Dec r
             Dec r -> Inc r
             Tgl v -> Inc v
             Stop -> Stop

current :: State -> Instr
current (_, p, i) = maybe Stop id (i V.!? p)

exec :: State -> State
exec = go
  where go !s =
          case current s of
            Jnz v steps     -> continue $ jnz s (val s v) ((val s steps)-1)
            Cpy v (Right r) -> continue $ cpy s (val s v) r
            Inc (Right r)   -> continue $ inc s r
            Dec (Right r)   -> continue $ dec s r
            Tgl v           -> continue $ tgl s (val s v)
            _               -> s

        {-# INLINE continue #-}
        continue s = go (forward s 1)

unsafeRight (Right x) = x

parseAll = map unsafeRight .
  map (parse instrP "") . lines

run input x =
  let (m, p, i) = newState input
      (regs, _, _) = exec (setReg A x m , p, i)
  in regVal regs A

part1 input = run input 7
part2 input = run input 12

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
