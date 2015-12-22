import Data.List (minimumBy, maximumBy)

data Item = Item { name :: !String
                 , cost :: !Int
                 , damage :: !Int
                 , armor :: !Int } deriving (Show, Eq)
data Stats = Stats { hp :: !Int
                   , dmg :: !Int
                   , def :: !Int
                   , spent :: !Int } deriving (Show)

weapons = [
  Item "Dagger" 8 4 0,
  Item "Shortsword" 10 5 0,
  Item "Warhammer" 25 6 0,
  Item "Longsword" 40 7 0,
  Item "Greataxe" 74 8 0]

armors = [
  Item "Leather" 13 0 1,
  Item "Chainmail" 31 0 2,
  Item "Splintmail" 53 0 3,
  Item "Bandedmail" 75 0 4,
  Item "Platemail" 102 0 5 ]

rings = [
  Item "Damage +1" 25 1 0,
  Item "Damage +2" 50 2 0,
  Item "Damage +3" 100 3 0,
  Item "Defense +1" 20 0 1,
  Item "Defense +2" 40 0 2,
  Item "Defense +3" 80 0 3 ]

noItem = Item "Empty slot" 0 0 0

costCompare i1 i2 = cost i1 `compare` cost i2
spentCompare p1 p2 = spent p1 `compare` spent p2

attack p1 p2 = max 0 (dmg p1 - def p2)

duel player boss
 | hp player <= 0 = False
 | hp boss <= 0 = True
 | otherwise = not $ duel (boss { hp = hp boss - attack player boss }) player

choices = do
  i <- weapons
  j <- armors
  k' <- rings
  l' <- filter (/= k') rings
  k <- [noItem, k']
  l <- [noItem, l']
  let items = [i, j, k , l]
  pure $ Stats { hp = 100
               , spent = sum $ map cost items
               , def = sum $ map armor items
               , dmg = sum $ map damage items}


input = Stats { hp = 103
              , dmg = 9
              , def = 2
              , spent = 0}
part1 = minimumBy spentCompare . filter (flip duel input) $ choices
part2 = maximumBy spentCompare . filter (duel input) $ choices

main = do
     print part1
     print part2
