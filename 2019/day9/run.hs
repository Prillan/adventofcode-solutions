{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Control.Monad.State ( StateT(..)
                           , gets
                           , modify
                           , lift
                           , get
                           , MonadState )
import Data.Bool (bool)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybe)
import Pipes
import qualified Pipes.Prelude as P

type Program = Map Integer Integer

data ExecInfo = ExecInfo { pc :: Integer
                         , halted :: Bool
                         , memory :: Program
                         , relativeBase :: Integer }
  deriving Show

data Address = Absolute Integer | Relative Integer
  deriving Show

data Instruction = Add Param Param Address
                 | Mul Param Param Address
                 | Input Address
                 | Output Param
                 | JumpNonZero Param Param
                 | JumpZero Param Param
                 | LessThan Param Param Address
                 | Equals Param Param Address
                 | ModifyRelativeBase Param
                 | Exit
  deriving Show

data Param = Position Address | Immediate Integer
  deriving Show


execInfo :: Program -> ExecInfo
execInfo program = ExecInfo { pc = 0
                            , memory = program
                            , relativeBase = 0
                            , halted = False }


resolveAddress :: MonadState ExecInfo m => Address -> m Integer
resolveAddress (Absolute a) = pure a
resolveAddress (Relative v) = (v +) <$> gets relativeBase

rawValAt :: MonadState ExecInfo m => Address -> m Integer
rawValAt addr =
  maybe 0 id <$> (Map.lookup <$> resolveAddress addr <*> gets memory)

modifyPc :: MonadState ExecInfo m => (Integer -> Integer) -> m ()
modifyPc f = modify (\s@ExecInfo { pc } -> s { pc = f pc })

modifyRelativeBase :: MonadState ExecInfo m => (Integer -> Integer) -> m ()
modifyRelativeBase f =
  modify (\s@ExecInfo { relativeBase } -> s { relativeBase = f relativeBase })

halt :: MonadState ExecInfo m => m ()
halt = modify (\s -> s { halted = True })

paramValue :: MonadState ExecInfo m => Param -> m Integer
paramValue (Immediate x) = pure x
paramValue (Position x) = rawValAt x

writeMemory :: MonadState ExecInfo m => Address -> Integer -> m ()
writeMemory addr val = do
  addr' <- resolveAddress addr
  modify (\s@ExecInfo { memory } -> s { memory = Map.insert addr' val memory })

parseAll :: String -> Program
parseAll input = Map.fromList $ zip [0..] . read $ '[' : input ++ "]"

consumeInstruction :: MonadState ExecInfo m => m Instruction
consumeInstruction = do
  rawInstruction <- rawValAt . Absolute =<< gets pc
  modifyPc (+ 1)
  case parseFullOpCode rawInstruction of
    (1, [pmx, pmy, pmz]) -> Add <$> consumeParam pmx
                              <*> consumeParam pmy
                              <*> consumeAddress pmz
    (2, [pmx, pmy, pmz]) -> Mul <$> consumeParam pmx
                              <*> consumeParam pmy
                              <*> consumeAddress pmz
    (3, pmx:_) -> Input <$> consumeAddress pmx
    (4, pmx:_) -> Output <$> consumeParam pmx
    (5, pmx:pmy:_) -> JumpNonZero <$> consumeParam pmx <*> consumeParam pmy
    (6, pmx:pmy:_) -> JumpZero <$> consumeParam pmx <*> consumeParam pmy
    (7, [pmx, pmy, pmz]) -> LessThan <$> consumeParam pmx
                                   <*> consumeParam pmy
                                   <*> consumeAddress pmz
    (8, [pmx, pmy, pmz]) -> Equals <$> consumeParam pmx
                                   <*> consumeParam pmy
                                   <*> consumeAddress pmz
    (9, pmx:_) -> ModifyRelativeBase <$> consumeParam pmx
    (99, _) -> pure Exit
    _ -> do
      state <- get
      error $ unlines $
        [ "Invalid opcode (instr: " ++ show rawInstruction ++ "),"
        , " parsed instruction: " ++ show (parseFullOpCode rawInstruction)
        , " state is: " ++ show state ]

parseFullOpCode :: Integer -> (Integer, [Integer])
parseFullOpCode x =
  let opCode = x `mod` 100
      paramModes = map (\d -> (x `div` d) `mod` 10) [100, 1000, 10000]
  in
    (opCode, paramModes)

exec :: Monad m => Program -> Pipe Integer Integer m ()
exec program = runStateT go (execInfo program) *> pure ()
  where go = do
          execNext
          h <- gets halted
          case h of
            True  -> pure ()
            False -> go

execNext :: Monad m => StateT ExecInfo (Pipe Integer Integer m) ()
execNext = do
  instruction <- consumeInstruction
  case instruction of
    Add px py outAddr -> binOp (+) px py >>= writeMemory outAddr
    Mul px py outAddr -> binOp (*) px py >>= writeMemory outAddr
    Input outAddr -> lift await >>= writeMemory outAddr
    Output px -> paramValue px >>= lift . yield
    JumpNonZero px pjump -> do
      jumpAddr <- paramValue pjump
      (/= 0) <$> paramValue px >>= branch jumpAddr
    JumpZero px pjump ->do
      jumpAddr <- paramValue pjump
      (== 0) <$> paramValue px >>= branch jumpAddr
    LessThan px py outAddr ->
      comparison (<) px py >>= writeMemory outAddr
    Equals px py outAddr ->
      comparison (==) px py >>= writeMemory outAddr
    ModifyRelativeBase px -> paramValue px >>= modifyRelativeBase . (+)
    Exit -> halt

branch :: MonadState ExecInfo m => Integer -> Bool -> m ()
branch jumpAddr = bool (pure ()) (modifyPc (const jumpAddr))

comparison :: MonadState ExecInfo m
  => (Integer -> Integer -> Bool)
  -> Param
  -> Param
  -> m Integer
comparison comp = binOp (\x y -> bool 0 1 (comp x y))

binOp :: MonadState ExecInfo m
  => (Integer -> Integer -> a)
  -> Param
  -> Param
  -> m a
binOp op px py = op <$> paramValue px <*> paramValue py

consumeValue :: MonadState ExecInfo m => m Integer
consumeValue = (gets pc >>= rawValAt . Absolute) <* modifyPc (+ 1)

consumeAddress :: MonadState ExecInfo m => Integer -> m Address
consumeAddress 0 = Absolute <$> consumeValue
consumeAddress 2 = Relative <$> consumeValue
consumeAddress _ = error "Impossible"


consumeParam :: MonadState ExecInfo m => Integer -> m Param
consumeParam 1 = Immediate <$> consumeValue
consumeParam x = Position <$> consumeAddress x

part1 :: Program -> Integer
part1 program = head . P.toList $ yield 1 >-> exec program


part2 :: Program -> Integer
part2 program = head . P.toList $ yield 2 >-> exec program

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
