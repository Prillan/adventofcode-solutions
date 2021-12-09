module Main (main) where

import Data.Maybe (maybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type Program = HashMap Integer Integer
type ExecInfo = (Integer, Program)

val :: Integer -> Program -> Integer
val x prog = maybe 0 id $ HashMap.lookup x prog

val2 :: Integer -> Program -> Integer
val2 x prog = val (val x prog) prog

parseAll :: String -> Program
parseAll input = HashMap.fromList $ zip [0..] . read $ '[' : input ++ "]"

exec :: Program -> Program
exec prog = exec' (0, prog)

exec' :: ExecInfo -> Program
exec' (pc, prog) =
  case val pc prog of
    1 -> exec' (binOp (+) (pc, prog))
    2 -> exec' (binOp (*) (pc, prog))
    99 -> prog
    _ -> error $ "Invalid opcode, state is: " ++ show (pc, prog)

binOp :: (Integer -> Integer -> Integer) -> ExecInfo -> ExecInfo
binOp (¤) (pc, prog) =
  let result = (val2 (pc + 1) prog) ¤ (val2 (pc + 2) prog)
  in
    (pc + 4, HashMap.insert (val (pc + 3) prog) result prog)

execWithInput :: Integer -> Integer -> Program -> Program
execWithInput x y = exec . HashMap.insert 1 x . HashMap.insert 2 y

part1 :: Program -> Integer
part1 = val 0 . execWithInput 12 2

part2 :: Program -> Integer
part2 prog =
  let inputs = (,) <$> [0..99] <*> [0..99]
      results = map (\(x, y) -> val 0 (execWithInput x y prog)) inputs
      ((noun, verb), _) =
        head . filter ((== 19690720) . snd) $ zip inputs results
  in
    noun * 100 + verb

main :: IO ()
main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
