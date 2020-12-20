{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
import AoC
import AoC.Grid

-- TODO: CLEAN-UP!!!!
-- DO NOT TRY TO UNDERSTAND THIS

import Control.Monad (guard)
import Data.Bits (xor)
import Data.Ord (comparing)
import Data.Bifunctor
import Data.Maybe
import Data.List
import Data.List.Split (splitOn)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Text.Read (readMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

import Data.Void (Void)

type Parser = Parsec Void String

-- 0: 4 1 5
-- 1: 2 3 | 3 2
-- 2: 4 4 | 5 5
-- 3: 4 5 | 5 4
-- 4: "a"
-- 5: "b"

-- ababbb
-- bababa
-- abbbab
-- aaabbb
-- aaaabbb

data Term = Lit Char
          | Ref Int
          -- | Seq [Term]
          -- | Alts [Term]
  deriving (Show, Eq)

type Rule = [[Term]]

ruleP = traverse altP . splitOn " | "
altP = traverse termP . words
termP x = (Lit . head <$> readMaybe x) <|> (Ref <$> readMaybe x)

parseRule xs = do
  (idx, ':':' ':rest) <- listToMaybe $ reads @Int xs
  (idx,) <$> ruleP rest

parseAll xs =
  let (rules, "":messages) = break (== "") $ lines xs
  in (IntMap.fromList $ map fromJust $ map parseRule rules, messages)

-- match :: IntMap Rule -> String -> Bool
-- match rules s =

validStrings :: IntMap Rule -> IntMap [String]
validStrings m = loeb ss
  where ss = IntMap.map f m
        f :: Rule -> IntMap [String] -> [String]
        f x m' =
          concat $ traverse (perChoice m') x

        perChoice :: IntMap [String]
                  -> [Term]
                  -> [String]
        perChoice m' =
          map concat . traverse (perTerm m')

        perTerm :: IntMap [String]
                -> Term
                -> [String]
        perTerm m' (Lit s) = [[s]]
        perTerm m' (Ref i) = m' IntMap.! i

--rules :: IntMap Rule -> [Parser ()]
rules m = loeb ss
  where ss = IntMap.mapWithKey f m
        f :: Int -> Rule -> IntMap (Parser ()) -> Parser ()
        f i x m' =
          try
          . foldl' (<|>) empty
          $ map (perChoice m') x

        perChoice :: IntMap (Parser ())
                  -> [Term]
                  -> Parser ()
        perChoice m' xs =
           try (traverse (perTerm m') xs *> pure ())

        perTerm :: IntMap (Parser ())
                -> Term
                -> Parser ()
        perTerm m' (Lit s) = char s *> pure ()
        perTerm m' (Ref i) = m' IntMap.! i <?> "rule " ++ show i

loeb :: Functor a => a (a x -> x) -> a x
loeb x = fmap (\a -> a (loeb x)) x

wrong =
  IntMap.fromList
  . map fromJust
  . map parseRule
  $ [ "8: 42 | 42 8"
    , "11: 42 31 | 42 11 31" ]

solve p msgs =
  length
  . filter (isJust . parseMaybe (p <* eof))
  $ msgs

part1 (r, msgs) = solve (rules r IntMap.! 0) msgs

part2 (r, msgs) =
  let rls = rules r
      p = modified (rls IntMap.! 42) (rls IntMap.! 31)
  in solve p msgs

main = main' "input.txt"
exampleMain = main' "example.txt"
example2Main = main' "example2.txt"

main' file = do
   input <- parseAll <$> readFile file
   print (part1 input)
   print (part2 input)


test = fst . parseAll <$> readFile "test.txt"
test0 = (<* eof) . (IntMap.! 0) . rules <$> test

modified p42 p31 = do
  c1 <- length <$> some p42
  c2 <- length <$> some p31
  guard $ c1 > c2
  pure ()

_0p :: Parser ()
_0p = (dbg "_0 8" _8p) *> (dbg "_0 11" _11p) *> eof *> pure ()
_8p = try (_42p *> _8p) <|> try _42p
_11p = try (_42p *> _31p) <|> try (_42p *> _11p *> _31p)
_42p = string "42"
_31p = string "31"

t = mapM_ (\x -> putStrLn x *> parseTest (modified _42p _31p) x) ["42", "4242", "424231", "42424231"]
