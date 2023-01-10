module AoC.Parse (numP) where

import Data.Maybe (maybeToList)

import Text.Megaparsec
import Text.Megaparsec.Char

numP :: (Num a, Read a, Ord e) => Parsec e String a
numP = do
  sign   <- optional (char '-')
  digits <- some digitChar
  case reads (maybeToList sign ++ digits) of
    [(val, "")] -> pure val
    [(_, _)] -> fail "failed to read num (leftover state)"
    [] -> fail "failed to read num (no parse)"
    _:_ -> fail "failed to read num (ambiguous parse)"
