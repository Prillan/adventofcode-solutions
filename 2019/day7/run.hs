{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.Identity (Identity(..))
import Control.Monad.State ( StateT(..)
                           , gets
                           , modify
                           , lift
                           , get
                           , MonadState )
import Data.Bool (bool)
import Data.List (permutations)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybe)
import Pipes
import qualified Pipes.Prelude as P

type Program = Map Integer Integer
type ExecInfo = (Integer, Bool, Program)

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

data Param = Position Address | Immediate Integer
  deriving Show

rawValAt :: MonadState ExecInfo m => Address -> m Integer
rawValAt addr = maybe 0 id . Map.lookup addr <$> gets memory

pc :: ExecInfo -> Integer
pc (pc', _, _) = pc'

modifyPc :: MonadState ExecInfo m => (Integer -> Integer) -> m ()
modifyPc f = modify (\(pc', halt, prog) -> (f pc', halt, prog))

memory :: ExecInfo -> Program
memory (_, _, m) = m

paramValue :: MonadState ExecInfo m => Param -> m Integer
paramValue (Immediate x) = pure x
paramValue (Position x) = rawValAt x

writeMemory :: MonadState ExecInfo m => Address -> Integer -> m ()
writeMemory addr val =
  modify (\(pc', halt, prog) -> (pc', halt, Map.insert addr val prog))

parseAll :: String -> Program
parseAll input = Map.fromList $ zip [0..] . read $ '[' : input ++ "]"

consumeInstruction :: MonadState ExecInfo m => m Instruction
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

exec :: Monad m => Program -> Pipe Integer Integer m ()
exec program = runStateT go (0, False, program) *> pure ()
  where halted (_, x, _) = x
        go = do
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
    Exit -> modify (\(x, _, y) -> (x, True, y))

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
consumeValue = (gets pc >>= rawValAt) <* modifyPc (+ 1)

consumeAddress :: MonadState ExecInfo m => m Address
consumeAddress = consumeValue

consumeParam :: MonadState ExecInfo m => Integer -> m Param
consumeParam 0 = Position <$> consumeValue
consumeParam 1 = Immediate <$> consumeAddress
consumeParam _ = error "Impossible"

ampChain :: Monad m => Program -> [Integer] -> Pipe Integer Integer m ()
ampChain program phases =
  let r x = (yield x >> cat) >-> exec program
  in
    foldl1 (>->) $ map r phases

runChain :: Pipe Integer Integer Identity () -> Integer
runChain chain = head . P.toList $ yield 0 >-> chain

runFeedbackChain :: Pipe Integer Integer Identity () -> Integer
runFeedbackChain chain = last $ loop []
  where loop inputs =
          let outputs = P.toList $ each inputs >-> chain
          in
            if (0:outputs) == inputs
              then inputs
              else loop (0:outputs)


highestOutput :: (Pipe Integer Integer Identity () -> Integer)
  -> [Integer]
  -> Program
  -> Integer
highestOutput chainRunner phases program =
  maximum . map (chainRunner . ampChain program) . permutations $ phases


part1 :: Program -> Integer
part1 = highestOutput runChain [0..4]


part2 :: Program -> Integer
part2 = highestOutput runFeedbackChain [5..9]

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
