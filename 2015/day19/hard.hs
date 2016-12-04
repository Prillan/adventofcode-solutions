{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import           Data.Aeson
import           Data.Char
import           Data.Coerce
import           Data.List (permutations, group, minimum, maximum, minimumBy, maximumBy, isPrefixOf, sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe, listToMaybe)
import qualified Data.Set as Set
import           Data.String
import           Data.Vector ((!), (!?))
import qualified Data.Vector as V
import Debug.Trace (trace)
import           Text.Parsec

data Atom = Al
          | Ar
          | B
          | C
          | Ca
          | F
          | H
          | Mg
          | N
          | O
          | P
          | Rn
          | Si
          | Th
          | Ti
          | Y
          | E
  deriving (Show, Eq, Read)

newtype Molecule = Molecule [Atom] deriving (Eq)
instance Show Molecule where
  show = concat . map show . (coerce :: Molecule -> [Atom])

data Rule = Rule !Atom !Molecule deriving Show

azl = ['a'..'z']
azu = ['A'..'Z']

atom = fmap (\v -> (read :: String -> Atom) v) ((:) <$> oneOf azu <*> many (oneOf azl))
molecule = Molecule <$> many1 atom
rule = Rule <$> atom <*> (string " => " *> molecule)

unsafeRight (Right x) = x

eUp 'e' = 'E'
eUp x = x
parseAll input = ( map unsafeRight . map (parse rule "") $ r
                 , unsafeRight . parse molecule "" . filter (`elem` (azl++azu)) $ l)
  where l = last . lines $ input
        r = init . map (map eUp) . filter ((>= 1) . length) . lines $ input


-- BASICALLY EVERYTHING I TRIED IS BELOW BEFORE I LOOKED AT OTHER PEOPLE'S SOLUTIONS

-- --replace :: Rule -> Molecule -> [Molecule]
-- replace (Molecule f) (Molecule t) (Molecule v) = map Molecule $ replace' [] v
--    where replace' _ [] = []
--          replace' l r@(rh:rs)
--            | f `isPrefixOf` r = ((reverse l) ++ t ++ (drop (length f) r)) : replace' (rh:l) rs
--            | otherwise = replace' (rh:l) rs

-- --replaceFirst :: String -> String -> String -> Maybe String
-- replaceFirst (Molecule f) (Molecule t) (Molecule v) = Molecule <$> replace' [] v
--    where replace' _ [] = Nothing
--          replace' l r@(rh:rs)
--            | f `isPrefixOf` r = Just ((reverse l) ++ t ++ (drop (length f) r))
--            | otherwise = replace' (rh:l) rs

-- -- applyMany input (Rule (Molecule from) (Molecule to)) = replace from to input
-- -- step :: [Rule] -> String -> Set.Set String
-- -- step rules input = Set.fromList $ concatMap (applyMany input) rules

-- reverseRule (Rule a (Molecule m)) = (Molecule $ reverse m, Molecule [a])
-- -- noE (Rule (Molecule m1) _) = m1 /= "e"
-- bigTo (Rule _ (Molecule m1)) (Rule _ (Molecule m2)) = length m2 `compare` length m1

-- -- part1 rules input = step rules input
-- -- part2 rules input = filter (\(i, s) -> trace (show i) $ Set.member input s) values
-- --   where step' :: Set.Set String -> Set.Set String
-- --         step' = Set.unions . map (Set.filter ((<= l) . length) . step rules) . Set.toList
-- --         values = zip [0..] $ iterate step' (Set.fromList ["e"])
-- --         l = length input

-- -- part2' rules input =
-- --   filter (\(i, (s)) -> trace (show i) $ not . Set.null $ Set.intersection s targets) values
-- --   where targets = Set.fromList ["HF", "Nal", "OMg"]
-- --         rules' = map reverseRule . filter noE . sortBy bigTo $ rules
-- --         step' :: (Set.Set String) -> (Set.Set String)
-- --         step' (toCheck) = (new)
-- --           where new = (
-- --                  Set.unions
-- --                  . map (step rules')
-- --                  . Set.toList $ toCheck)
-- --         values = zip [0..] $ iterate step' (Set.fromList [input])

-- part2'' :: [Rule] -> Molecule -> Maybe Int
-- part2'' rules (Molecule input) = solve 0 $ Molecule $ reverse $ input
--   where solve n input'
--           | Molecule [E] == input' = Just n
--           | otherwise = listToMaybe
--                       $ concatMap (\(f, t) -> mapMaybe (solve (n+1)) (replace f t input'))
--                       $ reversed
--         reversed = map reverseRule . sortBy bigTo $ rules

part2''' _ (Molecule input) = symbols - lefts - rights - (2*ys) - 1
  where symbols = length input
        lefts = length $ filter (== Rn) input
        rights = length $ filter (== Ar) input
        ys = length $ filter (== Y) input

main = do
   (rules, input) <- parseAll <$> readFile "input.txt"
--   mapM_ print rules
   -- print input
   -- print (length $ part1 rules input)
   print (part2''' rules input)
