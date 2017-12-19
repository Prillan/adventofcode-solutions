import Data.Foldable
import Data.Bits (xor)
import Data.Maybe
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence ( Seq(..)
                     , (|>)
                     , viewr
                     , ViewR(..)
                     , viewl
                     , ViewL(..)
                     , dropWhileL )
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
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
data Action = Play Integer | Receive Integer
  deriving Show
data ProgramState = PState { program       :: Map Int (Program ())
                           , pointer       :: Int
                           , programLength :: Int
                           , regs          :: Map Reg Integer
                           , actionLog     :: [Action] }

instance Show ProgramState where
  show s = "State { pointer = " ++ show (pointer s) ++ ", "
           ++ "regs = " ++ show (regs s) ++ ", "
           ++ "log = " ++ show (toList $ actionLog s) ++ " }"

type Program = State ProgramState

append :: Action -> ProgramState -> ProgramState
append a s = s { actionLog = a:actionLog s }

isPlay (Play _) = True
isPlay _ = False

receive :: Program (Maybe Integer)
receive = do
  actions <- gets actionLog
  case filter isPlay actions of
    Play freq:_  -> pure $ Just freq
    _ -> pure $ Nothing

setReg :: Reg -> Integer -> ProgramState -> ProgramState
setReg r v s = s { regs = Map.insert r v (regs s) }

regLookup :: Reg -> Map Reg Integer -> Integer
regLookup r = maybe 0 id . Map.lookup r

reg r = regLookup r . regs

play :: Reg -> Program ()
play r = do
  rv <- gets (reg r)
  modify' (append (Play rv))

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
operationP op s = do
  string s
  spaceChar
  x <- anyChar
  spaceChar
  y <- regValP
  pure $ operation op x y

sndP :: Parser (Program ())
sndP = do
  string "snd "
  reg <- anyChar
  pure $ play reg

rcvP :: Parser (Program ())
rcvP = do
  string "rcv "
  x <- regValP
  pure $ do
    vx <- x
    if vx /= 0
      then do
        val <- receive
        case val of
          Just f -> modify' (append (Receive f))
          Nothing -> error "No previous sound played"
      else pure ()

jgzP :: Parser (Program ())
jgzP = do
  string "jgz "
  x <- regValP
  spaceChar
  y <- regValP
  pure $ do
    vx <- x
    if vx > 0
      then do
        vy <- y
        modify (\s -> s { pointer = pointer s + fromInteger vy - 1 })
      else pure ()

setP :: Parser (Program ())
setP = do
  string "set "
  x <- anyChar
  spaceChar
  y <- regValP
  pure $ do
    vy <- y
    modify (setReg x vy)

addP :: Parser (Program ())
addP = operationP (+) "add"

mulP :: Parser (Program ())
mulP = operationP (*) "mul"

modP :: Parser (Program ())
modP = operationP mod "mod"

parseInstruction = sndP <|> setP <|> addP <|> mulP <|> modP <|> rcvP <|> jgzP

parseAll =
  map unsafeRight .
  map (parse parseInstruction "") . lines

eval :: Program (Maybe Integer)
eval = do
  s <- get
  case Map.lookup (pointer s) (program s) of
    Just instr -> do
      instr
      s' <- get
      case actionLog s' of
        Receive f:_ -> pure (Just f)
        _ -> put s' { pointer = pointer s' + 1 } >> eval
    Nothing -> pure Nothing

part1 :: [Program ()] -> Maybe Integer
part1 instr =
  let initial = PState { pointer = 0
                       , program = Map.fromList (zip [0..] instr)
                       , programLength = length instr
                       , regs = Map.empty
                       , actionLog = [] }
  in
    fst $ runState eval initial
--part2 = id

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
--   print (part2 input)
