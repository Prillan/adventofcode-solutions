{-# LANGUAGE DeriveFunctor #-}
import Control.Comonad
import qualified Data.Map.Strict as Map

data ListZipper a = LZ [a] a [a]
  deriving (Show, Functor)

instance Comonad ListZipper where
  extract = listRead
  duplicate = genericMove listLeft listRight

listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (l:ls) x rs) = LZ ls l (x:rs)
listLeft _ = error "listLeft"

listRight :: ListZipper a -> ListZipper a
listRight (LZ ls x (r:rs)) = LZ (x:ls) r rs
listRight _ = error "listRight"

listRead :: ListZipper a -> a
listRead (LZ _ x _) = x

zToList :: Int -> ListZipper a -> [a]
zToList n (LZ ls x rs) =
  reverse (take n ls) ++ [x] ++ take n rs

zToEnumList :: Int -> ListZipper a -> [(Int, a)]
zToEnumList n = zip [-n..n] . zToList n

genericMove :: (z a -> z a)
            -> (z a -> z a)
            -> z a
            -> ListZipper (z a)
genericMove a b z =
  LZ (iterate' a z) z (iterate' b z)

iterate' :: (a -> a) -> a -> [a]
iterate' f = tail . iterate f

data Cell = Empty | Occupied
  deriving (Show, Eq, Ord)

toCell :: Char -> Cell
toCell '#' = Occupied
toCell '.' = Empty
toCell x = error $ "Invalid char: " ++ show x

fromCell :: Cell -> Char
fromCell Empty = '.'
fromCell Occupied = '#'

type Pattern = (Cell, Cell, Cell, Cell, Cell)
data Rule = Rule { patt :: Pattern
                 , res  :: Cell }
  deriving Show

readRule :: String -> Rule
readRule (l2:l1:c:r1:r2:' ':'=':'>':' ':n:[]) =
  Rule ( toCell l2
       , toCell l1
       , toCell c
       , toCell r1
       , toCell r2 )
       (toCell n)
readRule x = error $ "Invalid rule: " ++ show x

type Game = ListZipper Cell

state :: Game -> Pattern
state (LZ (l1:l2:_) c (r1:r2:_)) = (l2, l1, c, r1, r2)
state _ = undefined

rule :: [Rule] -> Game -> Cell
rule rs g =
  case filter ((state g ==) . patt) rs of
    r:_ -> res r
    _   -> Empty

evolve :: [Rule] -> Game -> Game
evolve rs = extend (rule rs)

showGame :: Int -> Game -> [Char]
showGame n = map fromCell . zToList n

stripShow :: Int -> Game -> [Char]
stripShow n = map fromCell . strip . zToList n

strip :: [Cell] -> [Cell]
strip = reverse
  . dropWhile (== Empty)
  . reverse
  . dropWhile (== Empty)

readGame :: String -> Game
readGame [] =
  LZ (repeat Empty) Empty (repeat Empty)
readGame (c:rights) =
  LZ (repeat Empty) (toCell c) (map toCell rights ++ repeat Empty)

parseAll :: String -> (Int, Game, [Rule])
parseAll input =
  case lines input of
    g:_:rest ->
      let g' = drop (length "initial state: ") g
      in
        (length g', readGame g', map readRule rest)
    _ -> error "Invalid input"

findCycle :: Ord a => [a] -> (a, Int, Int)
findCycle = go Map.empty . zip [0..]
  where go _ [] = undefined -- Can't happen for infinite lists
        go m ((i, e):rest) =
          case Map.lookup e m of
            Just j  -> (e, j, i - j)
            Nothing -> go (Map.insert e i m) rest

runForever :: Game -> [Rule] -> [Game]
runForever game rules = iterate (evolve rules) game

value :: [(Int, Cell)] -> Integer
value = sum . map toInteger . map fst . filter ((Occupied ==) . snd)

part1 :: Int -> Int -> ListZipper Cell -> [Rule] -> Integer
part1 iterations startLength game rules =
  value
  . zToEnumList (startLength + iterations * 3)
  $ (runForever game rules) !! iterations

part2 :: Int -> Int -> Game -> [Rule] -> Integer
part2 iterations startLength game rules =
  let n = 100 * startLength
      (e, s, l) = findCycle
                  . map strip
                  . map (zToList n)
                  $ runForever game rules
      (leftMost1:leftMost2:_) =
        map (fst . head)
        . map (filter ((== Occupied) . snd))
        . map (zToEnumList n)
        . drop s
        $ runForever game rules
      lDiff = leftMost2 - leftMost1
      targetLeftMost = leftMost1 + lDiff * (iterations - s)
  in
    if l == 1
      then value $ zip [targetLeftMost..] e
      else error "Cycle is longer than 1 step, fix the code!"

main :: IO ()
main = do
   (startLength, startState, rules) <- parseAll <$> readFile "input.txt"
   print (part1 20 startLength startState rules)
   print (part2 50000000000 startLength startState rules)
