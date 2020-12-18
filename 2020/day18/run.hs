{-# LANGUAGE LambdaCase #-}
import AoC
import AoC.Grid

import Data.Maybe
import Data.List.Split (splitOn)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void (Void)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

operator :: Parser String
operator = choice (map (symbol . pure) "+*")

type Expr = [Term]
data Term = Num Int | Op String | Sub Expr
  deriving (Show, Eq)

numP :: Parser Term
numP = Num <$> lexeme L.decimal

opP :: Parser Term
opP = Op <$> operator

parensExprP :: Parser Term
parensExprP = Sub <$> parens exprP

exprP :: Parser Expr
exprP = many (try opP <|> try numP <|> parensExprP)

eval :: Expr -> Int
eval = go
  where -- Recursively reduce the first operator encountered.
        go (e1:Op "+":e2:rest) =
          go (Num (go [e1] + go [e2]):rest)
        go (e1:Op "*":e2:rest) =
          go (Num (go [e1] * go [e2]):rest)
        -- Eval the sub-expression
        go ((Sub expr):rest) =
          let n = go expr
          in go (Num n:rest)
        -- Stop when we only have a number left.
        go [Num n] = n


eval2 :: Expr -> Int
eval2 = go
  where -- Eval top level by splitting on the weakest operator,
        -- i.e. *, and taking the product of each evaluated factor.
        go = product . map go' . splitOn [Op "*"]
        -- Eval next level by summing the terms.
        go' = sum . map f
        -- At this point we're left with nums sub-expressions and the
        -- symbol '+'.
        f = \case
          Num n  -> n
          Sub e  -> go e
          Op "+" -> 0
          Op "*" -> error "wut"

parseAll :: String -> [Expr]
parseAll =
  map fromJust
  . map (parseMaybe exprP)
  . lines

part1 = sum . map eval
part2 = sum . map eval2

main = main' "input.txt"
exampleMain = main' "example.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)
