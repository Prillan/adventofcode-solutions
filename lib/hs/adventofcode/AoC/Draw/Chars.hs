module AoC.Draw.Chars (readLetters) where

import Data.List (transpose)
import Data.List.Split (chunksOf)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

readLetters :: String -> Either String String
readLetters =
  traverse readLetter
  . transpose
  . map (chunksOf 5)
  . lines
  . map normalizeChar

letters =
  let m = [ ('C', lc)
          , ('E', le)
          , ('G', lg)
          , ('H', lh)
          , ('J', lj)
          , ('K', lk)
          , ('L', ll)
          , ('P', lp)
          , ('R', lr)
          , ('U', lu)
          , ('Y', ly)
          , ('Z', lz)
          ]
  in ( HashMap.fromList m
     , HashMap.fromList (map (\(x, y) -> (y, x)) m)
     )

normalizeChar :: Char -> Char
normalizeChar = \case
  '#' -> 'X'
  '.' -> ' '
  c   -> c -- "\nX "

readLetter :: [String] -> Either String Char
readLetter block =
  let (_, m) = letters
      padded = map (\l -> take 5 (l ++ repeat ' ')) block
  in case HashMap.lookup padded m of
       Just l  -> Right l
       Nothing -> Left $ "No letter mapping found for this block: \n"
                        ++ unlines block


lc = [ " XX  "
     , "X  X "
     , "X    "
     , "X    "
     , "X  X "
     , " XX  "
     ]
le = [ "XXXX "
     , "X    "
     , "XXX  "
     , "X    "
     , "X    "
     , "XXXX "
     ]
lg = [ " XX  "
     , "X  X "
     , "X    "
     , "X XX "
     , "X  X "
     , " XXX "
     ]
lh = [ "X  X "
     , "X  X "
     , "XXXX "
     , "X  X "
     , "X  X "
     , "X  X "
     ]
lj = [ "  XX "
     , "   X "
     , "   X "
     , "   X "
     , "X  X "
     , " XX  "
     ]
lk = [ "X  X "
     , "X X  "
     , "XX   "
     , "X X  "
     , "X X  "
     , "X  X "
     ]
ll = [ "X    "
     , "X    "
     , "X    "
     , "X    "
     , "X    "
     , "XXXX "
     ]
lp = [ "XXX  "
     , "X  X "
     , "X  X "
     , "XXX  "
     , "X    "
     , "X    "
     ]
lr = [ "XXX  "
     , "X  X "
     , "X  X "
     , "XXX  "
     , "X X  "
     , "X  X "
     ]
lu = [ "X  X "
     , "X  X "
     , "X  X "
     , "X  X "
     , "X  X "
     , " XX  "
     ]
ly = [ "X   X"
     , "X   X"
     , " X X "
     , "  X  "
     , "  X  "
     , "  X  "
     ]
lz = [ "XXXX "
     , "   X "
     , "  X  "
     , " X   "
     , "X    "
     , "XXXX "
     ]


example =
  unlines [ "XXXX   XX X  X XXX  X  X  XX  XXX  X    X   X  XX "
          , "   X    X X  X X  X X X  X  X X  X X    X   X   X "
          , "  X     X XXXX X  X XX   X    X  X X     X X    X "
          , " X      X X  X XXX  X X  X    XXX  X      X     X "
          , "X    X  X X  X X X  X X  X  X X    X      X  X  X "
          , "XXXX  XX  X  X X  X X  X  XX  X    XXXX   X   XX  "
          ]
