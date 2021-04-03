import           Control.Monad.State
import           Control.Lens
import           Data.Vector ((!?))
import qualified Data.Vector as V
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import ProgramState

type Offset = Int
data Register = RA | RB deriving (Eq)
instance Show Register where
  show RA = "a"
  show RB = "b"
data Instruction = JIO !Register !Offset
                 | INC !Register
                 | TPL !Register
                 | HLF !Register
                 | JMP !Offset
                 | JIE !Register !Offset
  deriving (Show, Eq)

type Parser = Parsec Void String

offset :: Parser Int
offset = toInt <$> oneOf "+-" <*> some digitChar
  where toInt x xs
          | x == '+' = read xs
          | otherwise = read (x:xs)
register = (\x -> if x == 'a' then RA else RB) <$> oneOf "ab"

instruction :: Parser Instruction
instruction =
  choice $ map try [ jif JIO "jio"
                   , jif JIE "jie"
                   , jmp JMP "jmp"
                   , mod HLF "hlf"
                   , mod INC "inc"
                   , mod TPL "tpl"
                   ]
  where mod :: (Register -> Instruction) -> String -> Parser Instruction
        mod sym str = sym <$> (string (str ++ " ") *> register)
        jmp :: (Int -> Instruction) -> String -> Parser Instruction
        jmp sym str = sym <$> (string (str ++ " ") *> offset)
        jif :: (Register -> Int -> Instruction) -> String -> Parser Instruction
        jif sym str = sym <$> (string (str ++ " ") *> register) <*> (string ", " *> offset)

unsafeRight (Right x) = x

parseAll = V.fromList
         . map unsafeRight
         . map (parse instruction "")
         . lines

emulate a inst = runState eval (PState a 0 0)
  where eval = do
          i <- currentInstruction
          s <- get
          case i of
            Nothing -> pure ()
            Just (JIO r o) -> (if getR r s == 1 then rins += o else rins += 1) >> eval
            Just (INC r  ) -> modR r += 1  >> (rins += 1) >> eval
            Just (TPL r  ) -> modR r *= 3  >> (rins += 1) >> eval
            Just (HLF r  ) -> modR r ///= 2 >> (rins += 1) >> eval
            Just (JMP o  ) -> rins += o >> eval
            Just (JIE r o) -> (if even (getR r s) then rins += o else rins += 1) >> eval

        currentInstruction = (\s -> inst !? (_rins s)) <$> get
        x ///= y = x %= (`div` y)
        getR RA = _ra
        getR RB = _rb
        modR RA = ra
        modR RB = rb

part1 = _rb . snd . emulate 0
part2 = _rb . snd . emulate 1

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
