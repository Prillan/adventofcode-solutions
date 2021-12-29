{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

import Data.Int
import Control.Monad (guard)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

type N = Int8

-- TODO: Write up an explanation for WTH is happening here.

data Block = Push N N
           | Pop N N
  deriving (Show, Eq)

isPop :: Block -> Bool
isPop = \case Pop _ _ -> True
              _       -> False

pp :: N -> Block -> String
pp i = unlines . \case
  Push t1 t2 -> [ "if " ++ show t1 ++ " + z != w_" ++ show i
                , "  then push z; z = w_" ++ show i ++ " + " ++ show t2
                ]
  Pop  t1 t2 -> [ "if " ++ show t1 ++ " + z != w_" ++ show i
                , "  then z = w_" ++ show i ++ " + " ++ show t2
                , "  else pop z"
                ]

blocks :: [String] -> [Block]
blocks = map translate
         . filter (not . null)
         . splitOn ["inp w"]

translate :: [String] -> Block
translate =
  toOp
  . filter (/= 0)
  . mapMaybe (readMaybe @N)
  . concatMap words

toOp :: [N] -> Block
toOp [26, pop, term1, 25, 1, term2]
  | pop == 1  = Push term1 term2
  | otherwise = Pop  term1 term2
toOp mismatch = error $ "fail on: " ++ show mismatch

zval :: [N] -> N
zval = \case []  -> 0
             z:_ -> z

zset :: N -> [N] -> [N]
zset z = \case []   -> [z]
               _:zs -> z:zs

zpop :: [N] -> [N]
zpop = \case []   -> []
             _:zs -> zs

zpushset :: N -> [N] -> [N]
zpushset z = (z:)

findAll :: [N] -> [Block] -> [[N]]
findAll digits startBlocks = go 0 popBlocks [] [] startBlocks
  where go pushes remPops zs ws = \case
          [] | sum zs == 0 -> [reverse ws]
             | otherwise   -> []
          Pop t1 t2:bs -> do
            let z = zval zs
            if remPops > pushes
              then do
                w <- digits
                if t1 + z /= w
                  then go pushes (remPops - 1) (zset (w + t2) zs) (w:ws) bs
                  else go (pushes - 1) (remPops - 1) (zpop zs) (w:ws) bs
              else do
                w <- digits
                guard $ t1 + z == w
                go (pushes - 1) (remPops - 1) (zpop zs) (w:ws) bs
          Push t1 t2:bs -> do
            let z = zval zs
            if remPops > pushes
              then do
                w <- digits
                if t1 + z /= w
                  then go (pushes + 1) remPops (zpushset (w + t2) zs) (w:ws) bs
                  else go pushes remPops zs (w:ws) bs
              else do
                w <- digits
                guard $ t1 + z == w
                go pushes remPops zs (w:ws) bs
        popBlocks = length . filter isPop $ startBlocks

test = [Push 1 2, Pop 2 3]

parseAll :: String -> [Block]
parseAll = blocks . lines--map parse  . lines

solve :: [N] -> [Block] -> Integer
solve digits = read @Integer . concatMap show . head . findAll digits

part1 :: [Block] -> Integer
part1 = solve [9,8..1]

part2 :: [Block] -> Integer
part2 = solve [1..9]

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
