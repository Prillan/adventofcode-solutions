import Data.Foldable
import Data.Sequence ( Seq
                     , (!?)
                     , ViewL(..), (<|)
                     , ViewR(..), (|>)
                     , (><)
                     )
import qualified Data.Sequence as Seq
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char


data Dir = R | L
  deriving (Show, Eq)
data Inst = SwapPos Int Int
          | SwapLetter Char Char
          | Rotate Dir Int
          | RotatePos Char
          | Rev Int Int
          | Move Int Int

          | RevRotatePos Char
          | RevMove Int Int
  deriving (Show)

numP = read <$> some digitChar

swapPosP = SwapPos <$> (string "swap position " *> numP)
                   <*> (string " with position " *> numP)
swapLetterP = SwapLetter <$> (string "swap letter " *> asciiChar)
                         <*> (string " with letter " *> asciiChar)
rotateP = Rotate <$> (string "rotate " *> dirP)
                 <*> (char ' ' *> numP <* string " step")
  where dirP = (string "left" *> pure L)
           <|> (string "right" *> pure R)
rotatePosP = RotatePos <$> (string "rotate based on position of letter " *> asciiChar)
revP = Rev <$> (string "reverse positions " *> numP)
           <*> (string " through " *> numP)
moveP = Move <$> (string "move position " *> numP)
             <*> (string " to position " *> numP)

instP :: Parsec Void String Inst
instP = swapPosP
    <|> swapLetterP
    <|> try rotateP
    <|> rotatePosP
    <|> revP
    <|> moveP

unsafeRight (Right x) = x

parseAll = map unsafeRight .
  map (parse instP "") . lines

rev :: Inst -> Inst
rev (Rotate d x) =
  case d of
    L -> Rotate R x
    R -> Rotate L x
rev (RotatePos c) = RevRotatePos c
rev (Move x y) = RevMove x y
rev s@(Rev _ _) = s
rev s@(SwapPos _ _) = s
rev s@(SwapLetter _ _) = s
rev x = x

rotateL n seq =
  let go seq' = case Seq.viewl seq' of
                  EmptyL -> Seq.empty
                  (a :< rest) -> rest |> a
  in
    foldr (.) id (replicate n go) seq
rotateR n seq =
  let go seq' = case Seq.viewr seq' of
                  EmptyR -> Seq.empty
                  (a :> rest) -> rest <| a
  in
    foldr (.) id (replicate n go) seq


exec :: Seq Char -> Inst -> Seq Char
exec pw (SwapPos x y) =
  let Just (x', y') = (,) <$> pw !? x <*> pw !? y
  in
    Seq.update x y' $
    Seq.update y x' $ pw
exec pw (SwapLetter x y) = swap <$> pw
  where swap z
         | x == z    = y
         | y == z    = x
         | otherwise = z
exec pw (Rotate d s) =
  let rot = case d of
              R -> rotateR
              L -> rotateL
  in rot s pw
exec pw (RotatePos c) =
  let Just pos = Seq.elemIndexL c pw
      steps = 1 + pos + (if pos >= 4 then 1 else 0)
  in exec pw (Rotate R steps)
exec pw (Rev x y) =
  let (h, t) = Seq.splitAt x pw
      (th, tt) = Seq.splitAt (1 + y - x) t
  in h >< Seq.reverse th >< tt
exec pw (Move x y) =
  let Just c = pw !? x
  in Seq.insertAt y c $ Seq.deleteAt x pw
exec pw (RevRotatePos c) =
  let Just i = Seq.elemIndexL c pw
      t 1 = 0
      t 3 = 1
      t 5 = 2
      t 7 = 3
      t 2 = 4
      t 4 = 5
      t 6 = 6
      t 0 = 7
  in exec pw (Rotate L (8 + i - t i))
exec pw (RevMove x y)
  | x == y = pw
  | otherwise =
    let (x', y') = (min x y, max x y)
        (h, t) = Seq.splitAt x' pw
        (th, tt) = Seq.splitAt (1 + y' - x') t
        dir = if x < y
              then R
              else L
    in h >< exec th (Rotate dir 1) >< tt

-- Move x y | x < y -> [:x] + rotera L [x:y] + [y:]
-- => RevMove 

-- test (x, y) =
--   let start = Seq.fromList "abcdefgh"
--       woop = exec start (Move x y)
--       woopwoop = exec woop (RevMove x y)
--   in
--     ((x, y), (min x y, max x y), start==woopwoop, woop, woopwoop)


part1 = toList . foldl' exec (Seq.fromList start)
part2 = toList . foldl' exec (Seq.fromList stop) . reverse . map rev

start = "abcdefgh"
stop = "fbgdceah"

main = do
   input <- parseAll <$> readFile "input.txt"
   putStrLn (part1 input)
   putStrLn (part2 input)
