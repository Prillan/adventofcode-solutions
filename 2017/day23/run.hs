{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
import AoC.Parse (numP)
import Control.Monad (when)
import Control.Monad.State (State, execState, get, gets, modify')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Parser = Parsec Void String

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

type Reg = Char
data ProgramState = PState { program       :: HashMap Int (Program ())
                           , pointer       :: Int
                           , programLength :: Int
                           , regs          :: HashMap Reg Int
                           , operations    :: HashMap String Int }

type Program = State ProgramState

setReg :: Reg -> Int -> ProgramState -> ProgramState
setReg r v s = s { regs = HashMap.insert r v (regs s) }

regLookup :: Reg -> HashMap Reg Int -> Int
regLookup = HashMap.findWithDefault 0

incrementOp :: String -> ProgramState -> ProgramState
incrementOp str s =
  let ops = operations s
      prev = HashMap.findWithDefault 0 str ops
  in
    s { operations = HashMap.insert str (prev + 1) ops }

reg :: Reg -> ProgramState -> Int
reg r = regLookup r . regs

regValP :: Parser (Program Int)
regValP = (pure <$> numP) <|> (gets . reg <$> asciiChar)

operation :: (Int -> Int -> Int)
          -> Reg
          -> Program Int
          -> Program ()
operation op x y = do
  vx <- gets (reg x)
  vy <- y
  modify' (setReg x (vx `op` vy))

operationP :: (Int -> Int -> Int)
           -> String
           -> Parser (Program ())
operationP op str = do
  _ <- string str
  _ <- spaceChar
  x <- asciiChar
  _ <- spaceChar
  y <- regValP
  pure do
    operation op x y
    modify' (incrementOp str)

jnzP :: Parser (Program ())
jnzP = do
  _ <- string "jnz "
  x <- regValP
  _ <- spaceChar
  y <- regValP
  pure do
    vx <- x
    when (vx /= 0) do
      vy <- y
      modify' (\s -> s { pointer = pointer s + vy - 1 })
      modify' $ incrementOp "jnz"

setP :: Parser (Program ())
setP = do
  _ <- string "set "
  x <- asciiChar
  _ <- spaceChar
  y <- regValP
  pure do
    vy <- y
    modify' (incrementOp "set" . setReg x vy)

mulP :: Parser (Program ())
mulP = operationP (*) "mul"

subP :: Parser (Program ())
subP = operationP (-) "sub"

modP :: Parser (Program ())
modP = operationP mod "mod"

parseInstruction :: Parser (Program ())
parseInstruction =
  choice [ setP
         , mulP
         , modP
         , jnzP
         , subP
         ]

parseAll :: String -> [Program ()]
parseAll =
  map unsafeRight
  . map (parse parseInstruction "")
  . lines

eval :: Program ()
eval = do
  s <- get
  case HashMap.lookup (pointer s) (program s) of
    Just instr -> do
      instr
      modify' (\s' -> s' { pointer = pointer s' + 1 })
      eval
    Nothing -> pure ()

part1 :: [Program ()] -> Int
part1 instr =
  let initial = PState { pointer = 0
                       , program = HashMap.fromList (zip [0..] instr)
                       , programLength = length instr
                       , regs = HashMap.empty
                       , operations = HashMap.empty }
      final = execState eval initial
  in
    operations final HashMap.! "mul"

-- Translated code, not sure if it's even worth it to come up with a
-- general solution. See disassembled.py for the imperative version.
part2 :: a -> Int
part2 _ = go 1 108400 125400 0
  where go :: Int -> Int -> Int -> Int -> Int
        go !a !b !c !h =
         case (check b, b == c) of
           (False, False) -> go a (b + 17) c h
           (True, False)  -> go a (b + 17) c (h + 1)
           (False, True)  -> h
           (True, True)   -> h + 1
        check b = any (p b) [2..b-1]
        p b d =
          let (q, r) = b `divMod` d
          in r == 0 && q >= 2

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
