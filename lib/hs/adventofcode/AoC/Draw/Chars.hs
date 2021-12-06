module AoC.Draw.Chars (readLetters) where

import Data.List (transpose)
import Data.List.Split (chunksOf)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

readLetters :: String -> Either String String
readLetters =
  traverse readLetter
  . transpose
  . map (chunksOf 5)
  . lines

letters =
  let m = [ ('C', lc)
          , ('H', lh)
          , ('J', lj)
          , ('K', lk)
          , ('L', ll)
          , ('P', lp)
          , ('R', lr)
          , ('Y', ly)
          , ('Z', lz)
          ]
  in ( Map.fromList m
     , Map.fromList (map (\(x, y) -> (y, x)) m)
     )

readLetter :: [String] -> Either String Char
readLetter block =
  let (_, m) = letters
  in case Map.lookup block m of
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
