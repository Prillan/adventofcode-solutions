import           Data.List (permutations, group, minimum, maximum, minimumBy, maximumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.Megaparsec hiding (State)

type Reg = Char
type Val = Int
data Instr = Cpy (Either Val Reg) Reg
           | Jnz (Either Val Reg) Val
           | Inc Reg
           | Dec Reg
           | Stop
  deriving Show

numP = read <$> ((++) <$> many (char '-') <*> some digitChar)
regP = oneOf "abcd"

cpyP =
  Cpy <$> (string "cpy " *> (fmap Left numP <|> fmap Right regP))
      <*> (string " " *> regP)

jnzP =
  Jnz <$> (string "jnz " *> (fmap Left numP <|> fmap Right regP))
      <*> (string " " *> numP)

incP =
  Inc <$> (string "inc " *> regP)

decP =
  Dec <$> (string "dec " *> regP)

instrP :: Parsec Dec String Instr
instrP = cpyP <|> jnzP <|> incP <|> decP


data Zipper a = Z [a] a [a]
  deriving Show

current (Z _ c _) = c

left (Z [] _ _) = undefined
left (Z (l:ls) c rs) = Z ls l (c:rs)

right (Z _ _ []) = undefined
right (Z ls c (r:rs)) = Z (c:ls) r rs

end (Z _ _ []) = True
end (Z _ Stop _) = True
end _ = False

fromList :: a -> [a] -> Zipper a
fromList def xs =
  let (x':xs') = xs ++ [def]
  in Z [] x' xs'

type State = (Map Reg Val, Zipper Instr)

newState instr = (Map.fromList (zip "abcd" (repeat 0)), fromList Stop instr)

val :: State -> Either Val Reg -> Val
val _ (Left v) = v
val (m, _) (Right r) = maybe 0 id (Map.lookup r m)

iter :: (a -> a) -> Int -> a -> a
iter f n = foldr (.) id (replicate n f)

forward :: State -> Val -> State
forward s 0 = s
forward (regs, z) n = (regs, iter right n z)

backward :: State -> Val -> State
backward s 0 = s
backward (regs, z) n = (regs, iter left n z)

jnz :: State -> Val -> Val -> State
jnz s v steps
  | v == 0               = s
  | v /= 0 && steps >= 0 = forward s steps
  | v /= 0 && steps <  0 = backward s (negate steps)

cpy :: State -> Val -> Reg -> State
cpy (m, z) v k  = (Map.insert k v m, z)

inc :: State -> Reg -> State
inc (m, z) k = (Map.update (pure . (+1)) k m, z)

dec :: State -> Reg -> State
dec (m, z) k = (Map.update (pure . (+ (-1))) k m, z)

exec1 :: State -> State
exec1 s =
  let s' = case current (snd s) of
             Jnz v steps -> jnz s (val s v) (steps-1)
             Cpy v r -> cpy s (val s v) r
             Inc r -> inc s r
             Dec r -> dec s r
             Stop -> backward s 1
  in
    forward s' 1

exec :: State -> State
exec = head . dropWhile (not.end.snd) . iterate exec1

unsafeRight (Right x) = x

parseAll = map unsafeRight .
  map (parse instrP "") . lines

part1 = exec . newState
part2 = exec . newState . ((Cpy (Left 1) 'c'):)

showState :: State -> String
showState (st, Z prev c next) =
  unlines $ [show st]
         ++ map (("  " ++) . show) (reverse prev)
         ++ ["→ " ++ show c]
         ++ map (("  " ++) . show) next

stepThrough :: [Instr] -> IO ()
stepThrough = (>> pure ())
            . mapM_ (\st -> getLine >> putStrLn (showState st))
            . iterate exec1 . newState

main = do
   input <- parseAll <$> readFile "input.txt"
   print (part1 input)
   print (part2 input)
