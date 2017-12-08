{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Maybe (maybe)
import Data.List (maximumBy, nub)
import Data.Ord (comparing)
import Data.Map (Map)
import qualified Data.Map as Map

import Text.Megaparsec

data Inst = Inst Op Reg Int Cond

instance Show Inst where
  show (Inst op (Reg reg) val cond) =
    show op ++ " " ++ show val ++ " " ++ reg ++ " cond"

data Op = Inc | Dec
  deriving (Show, Eq)

inst :: Inst -> Int -> Int
inst (Inst Dec _ val _) = (\x -> x - val)
inst (Inst Inc _ val _) = (+ val)

newtype Reg = Reg String
  deriving (Show, Eq, Ord)

data Cond = Cond Reg CompOp

type CompOp = Int -> Bool

type Parser = Parsec Dec String

rights = map f
  where f (Left x) = error $ show x
        f (Right x) = x

constP :: String -> a -> Parser a
constP str v = string str *> pure v

regP = Reg <$> some alphaNumChar
opP = constP "inc" Inc <|> constP "dec" Dec

compOpP :: Parser (Int -> Int -> Bool)
compOpP =
  constP ">=" (>=)
  <|> constP "<=" (<=)
  <|> constP ">" (>)
  <|> constP "==" (==)
  <|> constP "<" (<)
  <|> constP "!=" (/=)

numP = do
  sign <- maybe 1 (const $ -1) <$> optional (char '-')
  num <- read <$> some digitChar
  pure $ sign * num

instrP = do
  reg <- regP
  space
  op <- opP
  space
  val <- numP
  _ <- string " if "
  compReg <- regP
  space
  compOp <- compOpP
  space
  compTo <- numP
  pure $ Inst op reg val (Cond compReg (`compOp` compTo))

parseAll = rights . map (parse instrP "") . lines

lookupDefault :: (Ord k) => Map k a -> k -> a -> a
lookupDefault m k d = maybe d id (Map.lookup k m)

eval = scanl step Map.empty
  where step m i@(Inst op r v (Cond r' p))
          | p (lookupDefault m r' 0) =
            let f = inst i
            in
              Map.insertWith (\_ o -> f o) r (f 0) m
          | otherwise = m

part1 = maximumBy (comparing snd) . Map.toList . last . eval
part2 = maximumBy (comparing snd) . concatMap Map.toList . eval

main = do
  input <- parseAll <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
