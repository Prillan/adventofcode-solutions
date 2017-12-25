import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Text.Megaparsec ( Parsec
                       , anyChar
                       , digitChar
                       , string
                       , parse
                       , Dec
                       , spaceChar
                       , (<|>)
                       , some
                       , optional
                       , char )

type Parser = Parsec Dec String

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

type Reg = Char
data ProgramState = PState { program       :: Map Int (Program ())
                           , pointer       :: Int
                           , programLength :: Int
                           , regs          :: Map Reg Integer
                           , operations    :: Map String Integer }

instance Show ProgramState where
  show s = "State { pointer = " ++ show (pointer s) ++ ", "
           ++ "regs = " ++ show (regs s) ++ ", "
           ++ "ops = " ++ show (operations s) ++ " }"

type Program = State ProgramState

setReg :: Reg -> Integer -> ProgramState -> ProgramState
setReg r v s = s { regs = Map.insert r v (regs s) }

regLookup :: Reg -> Map Reg Integer -> Integer
regLookup r = maybe 0 id . Map.lookup r

incrementOp :: String -> ProgramState -> ProgramState
incrementOp str s =
  let ops = operations s
      prev = Map.findWithDefault 0 str ops
  in
    s { operations = Map.insert str (prev + 1) ops }

reg :: Reg -> ProgramState -> Integer
reg r = regLookup r . regs

numP :: Parser Integer
numP = do
  sign <- maybe 1 (const $ -1) <$> optional (char '-')
  num <- read <$> some digitChar
  pure $ sign * num

regValP :: Parser (Program Integer)
regValP = (pure <$> numP) <|> (gets . reg <$> anyChar)

operation :: (Integer -> Integer -> Integer)
          -> Reg
          -> Program Integer
          -> Program ()
operation op x y = do
  vx <- gets (reg x)
  vy <- y
  modify' (setReg x (vx `op` vy))

operationP :: (Integer -> Integer -> Integer)
           -> String
           -> Parser (Program ())
operationP op str = do
  string str
  spaceChar
  x <- anyChar
  spaceChar
  y <- regValP
  pure $ do
    operation op x y
    modify' (incrementOp str)

jnzP :: Parser (Program ())
jnzP = do
  string "jnz "
  x <- regValP
  spaceChar
  y <- regValP
  pure $ do
    vx <- x
    if vx /= 0
      then do
        vy <- y
        modify' (\s -> s { pointer = pointer s + fromInteger vy - 1 })
        modify' $ incrementOp "jnz"
      else pure ()

setP :: Parser (Program ())
setP = do
  string "set "
  x <- anyChar
  spaceChar
  y <- regValP
  pure $ do
    vy <- y
    modify' (incrementOp "set" . setReg x vy)

mulP :: Parser (Program ())
mulP = operationP (*) "mul"

subP :: Parser (Program ())
subP = operationP (-) "sub"

modP :: Parser (Program ())
modP = operationP mod "mod"

parseInstruction :: Parser (Program ())
parseInstruction = setP <|> mulP <|> modP <|> jnzP <|> subP

parseAll :: String -> [Program ()]
parseAll =
  map unsafeRight .
  map (parse parseInstruction "") . lines

eval :: Program ()
eval = do
  s <- get
  case Map.lookup (pointer s) (program s) of
    Just instr -> do
      instr
      modify' (\s' -> s' { pointer = pointer s' + 1 })
      eval
    Nothing -> pure ()

part1 :: [Program ()] -> ((), ProgramState)
part1 instr =
  let initial = PState { pointer = 0
                       , program = Map.fromList (zip [0..] instr)
                       , programLength = length instr
                       , regs = Map.empty
                       , operations = Map.empty }
  in
    runState eval initial

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
