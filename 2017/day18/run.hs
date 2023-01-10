{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
import Control.Monad (when)
import Control.Monad.State (State, execState, get, gets, modify', put)
import Data.Void (Void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence ( Seq
                     , (|>)
                     , viewl
                     , ViewL(..) )
import qualified Data.Sequence as Seq
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Parser = Parsec Void String

unsafeRight :: Show a => Either a b -> b
unsafeRight (Right x) = x
unsafeRight (Left x) = error $ show x

type Reg = Char
data ProcessState = Receiving
                  | Running
                  | Sending Integer
  deriving Show
data ProgramState = PState { program       :: IntMap (Program ())
                           , pointer       :: Int
                           , programLength :: Int
                           , regs          :: Map Reg Integer
                           , queue         :: Seq Integer
                           , processState  :: ProcessState
                           , sentCount     :: Integer
                           }

type Program = State ProgramState


setState :: ProcessState -> ProgramState -> ProgramState
setState p s = s { processState = p }

append :: Integer -> ProgramState -> ProgramState
append a s = s { queue = queue s |> a }

dequeue :: Program (Maybe Integer)
dequeue = do
  s <- get
  case viewl (queue s) of
    x :< rest -> do
      put s { queue = rest }
      pure (Just x)
    _ ->
      pure Nothing

receive :: Reg -> Program ()
receive r = dequeue >>= \case
  Just i -> do
    modify' (setReg r i)
    modify' (setState Running)
  Nothing ->
    modify' (setState Receiving)

setReg :: Reg -> Integer -> ProgramState -> ProgramState
setReg r v s = s { regs = Map.insert r v (regs s) }

regLookup :: Reg -> Map Reg Integer -> Integer
regLookup = Map.findWithDefault 0

reg :: Reg -> ProgramState -> Integer
reg r = regLookup r . regs

send :: Integer -> Program ()
send i = modify' (setState $ Sending i)

numP :: Parser Integer
numP = read <$> ((++) <$> many (char '-') <*> some digitChar)

regValP :: Parser (Program Integer)
regValP = (pure <$> numP) <|> (gets . reg <$> asciiChar)

operation :: (Integer -> Integer -> Integer)
          -> Reg
          -> Program Integer
          -> Program ()
operation op x y = do
  vx <- gets (reg x)
  vy <- y
  modify' $ setReg x (vx `op` vy)

operationP :: (Integer -> Integer -> Integer)
           -> String
           -> Parser (Program ())
operationP op s = do
  _ <- string s
  _ <- spaceChar
  x <- asciiChar
  _ <- spaceChar
  y <- regValP
  pure $ operation op x y

sndP :: Parser (Program ())
sndP = do
  _ <- string "snd "
  reg <- regValP
  pure $ reg >>= send

rcvP :: Parser (Program ())
rcvP = do
  _ <- string "rcv "
  x <- asciiChar
  pure $ receive x

jgzP :: Parser (Program ())
jgzP = do
  _ <- string "jgz "
  x <- regValP
  _ <- spaceChar
  y <- regValP
  pure do
    vx <- x
    when (vx > 0) do
      vy <- y
      modify' (\s -> s { pointer = pointer s + fromInteger vy - 1 })

setP :: Parser (Program ())
setP = do
  _ <- string "set "
  x <- asciiChar
  _ <- spaceChar
  y <- regValP
  pure do
    vy <- y
    modify' (setReg x vy)

addP :: Parser (Program ())
addP = operationP (+) "add"

mulP :: Parser (Program ())
mulP = operationP (*) "mul"

modP :: Parser (Program ())
modP = operationP mod "mod"

parseInstruction :: Parser (Program ())
parseInstruction =
  choice [ sndP
         , setP
         , addP
         , mulP
         , modP
         , rcvP
         , jgzP
         ]

parseAll :: String -> [Program ()]
parseAll =
  map unsafeRight
  . map (parse parseInstruction "")
  . lines

eval :: (ProgramState, ProgramState) -> (Bool, (ProgramState, ProgramState))
eval (p1, p2) =
  case (processState p1, processState p2) of
    (Receiving, Receiving) -> (False, (p1, p2))
    (Running,   _) -> (True, (stepProcess p1, p2))
    (Sending _, _) -> (True, send' p1 p2)
    (_, Sending _) -> let (p2', p1') = send' p2 p1
                      in
                        (True, (p1', p2'))
    (_, _)   -> (True, (p1, stepProcess p2))

send' :: ProgramState -> ProgramState -> (ProgramState, ProgramState)
send' p1 p2 =
  case processState p1 of
    Sending i -> ( setState Running p1 { sentCount = sentCount p1 + 1 }
                 , setState Running (append i p2))
    _ -> (p1, p2)

stepProcess :: ProgramState -> ProgramState
stepProcess p = flip execState p do
  case IntMap.lookup (pointer p) (program p) of
    Just instr -> do
      instr
      p' <- get
      case processState p' of
        Receiving -> pure ()
        _ -> put p' { pointer = pointer p' + 1 }
    Nothing -> pure ()

initial :: [Program ()] -> Integer -> ProgramState
initial prog pid = PState { pointer = 0
                          , program = IntMap.fromList (zip [0..] prog)
                          , programLength = length prog
                          , regs = Map.singleton 'p' pid
                          , queue = Seq.empty
                          , processState = Running
                          , sentCount = 0 }

part1 :: [Program ()] -> Integer
part1 instr = go 0 $ initial instr 0
  where go freq state =
          let state' = stepProcess state
          in case processState state' of
            Running   -> go freq state'
            Sending f -> go f state'
            Receiving -> freq

part2 :: [Program ()] -> Integer
part2 instr = go (initial instr 0, initial instr 1)
  where go ps =
          case eval ps of
            (False, (_, p2)) -> sentCount p2
            (True, ps') -> go ps'

main :: IO ()
main = do
  input <- parseAll <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
