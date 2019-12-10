{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- module Main (main) where

import Data.Bool (bool)
import Control.Monad.Writer (MonadWriter, Writer, execWriter, tell)
import Control.Monad.Identity (Identity(..))
import Control.Monad.State (StateT(..), gets, modify, lift, get, MonadState, MonadTrans, State, runState)
import Data.Char (isDigit)
import Data.Maybe (maybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Program = Map Integer Integer
type ExecInfo = (Integer, Program)

class Monad m => Interpreter m where
  readInput :: m Integer
  writeOutput :: Integer -> m ()

type Address = Integer
data Instruction = Add Param Param Address
                 | Mul Param Param Address
                 | Input Address
                 | Output Param
                 | JumpNonZero Param Param
                 | JumpZero Param Param
                 | LessThan Param Param Address
                 | Equals Param Param Address
                 | Exit
  deriving Show

instance Interpreter IO where
  readInput = do
    putStr "Input: "
    read . filter (/= '\n') <$> getLine
  writeOutput x = putStrLn $ "Output: " ++ show x

type ProgT = StateT ExecInfo

runProgT :: ProgT m a -> ExecInfo -> m (a, ExecInfo)
runProgT = runStateT

data Param = Position Address | Immediate Integer
  deriving Show

rawValAt :: MonadState ExecInfo m => Address -> m Integer
rawValAt addr = maybe 0 id . Map.lookup addr <$> gets memory

pc :: ExecInfo -> Integer
pc = fst

modifyPc :: MonadState ExecInfo m => (Integer -> Integer) -> m ()
modifyPc f = modify (\(pc, prog) -> (f pc, prog))

memory :: ExecInfo -> Program
memory = snd

paramValue (Immediate x) = pure x
paramValue (Position x) = rawValAt x

writeMemory addr val =
  modify (\(pc, prog) -> (pc, Map.insert addr val prog))

parseAll :: String -> Program
parseAll input = Map.fromList $ zip [0..] . read $ '[' : input ++ "]"

consumeInstruction :: Interpreter m => ProgT m Instruction
consumeInstruction = do
  rawInstruction <- rawValAt =<< gets pc
  modifyPc (+ 1)
  case parseFullOpCode rawInstruction of
    (1, [pmx, pmy, 0]) -> Add <$> consumeParam pmx
                              <*> consumeParam pmy
                              <*> consumeAddress
    (2, [pmx, pmy, 0]) -> Mul <$> consumeParam pmx
                              <*> consumeParam pmy
                              <*> consumeAddress
    (3, 0:_) -> Input <$> consumeAddress
    (4, pmx:_) -> Output <$> consumeParam pmx
    (5, pmx:pmy:_) -> JumpNonZero <$> consumeParam pmx <*> consumeParam pmy
    (6, pmx:pmy:_) -> JumpZero <$> consumeParam pmx <*> consumeParam pmy
    (7, [pmx, pmy, 0]) -> LessThan <$> consumeParam pmx
                                   <*> consumeParam pmy
                                   <*> consumeAddress
    (8, [pmx, pmy, 0]) -> Equals <$> consumeParam pmx
                                 <*> consumeParam pmy
                                 <*> consumeAddress
    (99, _) -> pure Exit
    _ -> do
      state <- get
      error $ "Invalid opcode (instr: "
               ++ show rawInstruction ++ "), state is: "
               ++ show state

parseFullOpCode :: Integer -> (Integer, [Integer])
parseFullOpCode x =
  let opCode = x `mod` 100
      paramModes = map (\d -> (x `div` d) `mod` 10) [100, 1000, 10000]
  in
    (opCode, paramModes)

exec :: Interpreter m => Program -> m Program
exec prog = snd . snd <$> runProgT exec' (0, prog)

exec' :: Interpreter m => ProgT m ()
exec' = do
  instruction <- consumeInstruction
  case instruction of
    Add px py outAddr -> binOp (+) px py >>= writeMemory outAddr >> exec'
    Mul px py outAddr -> binOp (*) px py >>= writeMemory outAddr >> exec'
    Input outAddr -> lift readInput >>= writeMemory outAddr >> exec'
    Output px -> paramValue px >>= lift . writeOutput >> exec'
    JumpNonZero px pjump -> do
      jumpAddr <- paramValue pjump
      (/= 0) <$> paramValue px >>= branch jumpAddr >> exec'
    JumpZero px pjump ->do
      jumpAddr <- paramValue pjump
      (== 0) <$> paramValue px >>= branch jumpAddr >> exec'
    LessThan px py outAddr ->
      comparison (<) px py >>= writeMemory outAddr >> exec'
    Equals px py outAddr ->
      comparison (==) px py >>= writeMemory outAddr >> exec'
    Exit -> pure ()

branch jumpAddr = bool (pure ()) (modifyPc (const jumpAddr))

comparison comp = binOp (\x y -> bool 0 1 (comp x y))

binOp :: MonadState ExecInfo m
  => (Integer -> Integer -> a)
  -> Param
  -> Param
  -> m a
binOp op px py = op <$> paramValue px <*> paramValue py

consumeValue :: MonadState ExecInfo m => m Integer
consumeValue = (gets pc >>= rawValAt) <* modifyPc (+ 1)

consumeAddress :: MonadState ExecInfo m => m Address
consumeAddress = consumeValue

consumeParam :: MonadState ExecInfo m => Integer -> m Param
consumeParam 0 = Position <$> consumeValue
consumeParam 1 = Immediate <$> consumeAddress

newtype Diagnostics a = Diagnostics (State (Integer, [Integer]) a)
  deriving ( Monad
           , Functor
           , Applicative
           , MonadState (Integer, [Integer]) )


runDiagnostics :: Integer -> Diagnostics a -> [Integer]
runDiagnostics mode (Diagnostics x) =
  snd . snd $ runState x (mode, [])

instance Interpreter Diagnostics where
  readInput = gets fst
  writeOutput x = modify (\(c, outputs) -> (c, x:outputs))



part1 :: Program -> Integer
part1 = head . runDiagnostics 1 . exec

part2 :: Program -> Integer
part2 = head . runDiagnostics 5 . exec

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
