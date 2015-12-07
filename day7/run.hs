{-# LANGUAGE FlexibleContexts #-}

import Debug.Trace (trace)

import Data.Bits
import Data.Functor.Identity (Identity)
import Data.Foldable (foldl')
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word16)
import Text.Parsec

type W = Word16

newtype Register = Register String
  deriving (Show, Eq, Ord)
data Value = Wire Register | Number W
  deriving Show
data Instruction = I Command Register
  deriving Show
data Command = SET Value
             | AND Value Value
             | OR Value Value
             | LSHIFT Value Int
             | RSHIFT Value Int
             | NOT Value
  deriving Show

cmd (I c _) = c

number :: Num a => ParsecT String u Identity a
number = fromInteger . (read :: String -> Integer) <$> many1 digit

register = Register <$> many1 alphaNum

val = (Number <$> number) <|> (Wire <$> register)

command =  try (AND <$> val <*> (string " AND " *> val))
       <|> try (OR <$> val <*> (string " OR " *> val))
       <|> try (NOT <$> (string "NOT " *> val))
       <|> try (LSHIFT <$> val <*> (string " LSHIFT " *> number))
       <|> try (RSHIFT <$> val <*> (string " RSHIFT " *> number))
       <|> try (SET <$> val)

instruction = I <$> command <*> (string " -> " *> register)

readInstruction = parse instruction ""

mapEither f = mapMaybe (g . f)
  where g (Left _) = Nothing
        g (Right x) = Just x

type CState = Map Register Word16

initial :: CState
initial = Map.empty

safeHead = listToMaybe . take 1
safeTail = drop 1

isOutput :: Register -> Instruction -> Bool
isOutput r (I _ o) = r == o

step ([], m) = ([], m)
step (xs, m) = foldl' f ([], m) xs
 where f (acc, m') (i@(I c o)) =
         case eval' c of
           Just v -> (acc, Map.insert o v m')
           Nothing -> (i:acc, m')
         where eval' :: Command -> Maybe W
               eval' (SET x) = get x
               eval' (AND x y) = (.&.) <$> get x <*> get y
               eval' (OR x y) = (.|.) <$> get x <*> get y
               eval' (NOT x) = complement <$> get x
               eval' (LSHIFT x n) = shiftL <$> get x <*> pure n
               eval' (RSHIFT x n) = shiftR <$> get x <*> pure n
               get (Wire x) = Map.lookup x m
               get (Number x) = pure x

getFinalState is = iterate step (is, initial)

process input = Map.lookup (Register "a") final
  where final = snd
              . head
              . dropWhile (not . null . fst)
              . getFinalState
              . mapEither readInstruction . lines $ input

main = do
   input <- readFile "input.txt"
   print (process input)
