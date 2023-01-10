{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
import AoC.Parse (numP)

import Control.Monad (replicateM)
import Data.Functor (($>))
import Data.Void ( Void )
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Text.Megaparsec
import Text.Megaparsec.Char

type N = Integer

type Parser = Parsec Void String

type Input a = HashMap String (Expr a)
type Row a = (String, Expr a)

monkeyP :: Parser String
monkeyP = replicateM 4 letterChar

opP :: (Integral a, Num a) => Parser (Expr a)
opP = do
  a <- monkeyP
  _ <- char ' '
  o <- choice [ char '*' $> (:*:)
              , char '+' $> (:+:)
              , char '-' $> (:-:)
              , char '/' $> (:/:)
              ]
  _ <- char ' '
  b <- monkeyP
  pure $ Var a `o` Var b

lineP :: (Read a, Num a, Integral a) => Parser (Row a)
lineP = do
  name <- monkeyP
  _ <- string ": "
  op <- choice
        [ Val <$> numP
        , opP
        ]
  pure (name, op)

data Expr a = Expr a :+: Expr a
            | Expr a :*: Expr a
            | Expr a :-: Expr a
            | Expr a :/: Expr a
            | Expr a :==: Expr a
            | Val a
            | Var String
  deriving (Show, Functor, Eq)

eval :: (Eq a, Fractional a, Num a) => HashMap String a -> Expr a -> a
eval vars = go
  where go = \case
          a :+: b -> go a + go b
          a :*: b -> go a * go b
          a :-: b -> go a - go b
          a :/: b -> go a / go b
          Var x
            | Just v <- vars HashMap.!? x -> v
            | otherwise -> error $ "Missing var: " ++ x
          Val a   -> a
          a :==: b -> error "Cannot eval :==:"

parseAll :: String -> HashMap String (Expr N)
parseAll =
  HashMap.fromList
  . map ((\(Right x) -> x) . parse lineP "") . lines

replace :: HashMap String (Expr a) -> Expr a -> Expr a
replace vars = go
  where go =
          \case Var x
                  | Just v <- vars HashMap.!? x -> v
                  | otherwise -> Var x
                a :+: b -> go a :+: go b
                a :*: b -> go a :*: go b
                a :-: b -> go a :-: go b
                a :/: b -> go a :/: go b
                a :==: b -> go a :==: go b
                Val v -> Val v


tie :: Input a -> HashMap String (HashMap String (Expr a) -> Expr a)
tie =
  HashMap.map (flip replace)

loeb :: Functor a => a (a x -> x) -> a x
loeb x = fmap (\a -> a (loeb x)) x

part1 :: Input N -> N
part1 =
  round
  . eval HashMap.empty
  . fmap toRational
  . (HashMap.! "root")
  . loeb
  . tie

setEq :: Expr a -> Expr a
setEq =
  \case a :+: b -> a :==: b
        a :*: b -> a :==: b
        a :-: b -> a :==: b
        a :/: b -> a :==: b
        x       -> x

part2 :: HashMap String (Expr N) -> N
part2 input =
  let lhs :==: rhs =
        fmap toRational
        . (HashMap.! "root")
        . loeb
        . tie
        . HashMap.adjust setEq "root"
        . HashMap.insert "humn" (Var "x")
        $ input
      line x = eval (HashMap.singleton "x" x) (rhs :-: lhs)

      m = line 0
      k = line 1 - m
  in round $ negate $ m / k

main :: IO ()
main = main' "input.txt"

exampleMain :: IO ()
exampleMain = main' "example.txt"

main' :: FilePath -> IO ()
main' file = do
  input <- parseAll <$> readFile file
  print (part1 input)
  print (part2 input)
