
import Data.Aeson
import Text.Parsec


data Ingredient = I String Int Int Int Int Int

ingredients = [
   I "Sprinkles" 5 (-1) 0 0 5,
   I "PeanutButter" (-1) 3 0 0 1,
   I "Frosting" 0 (-1) 4 0 6,
   I "Sugar" (-1) 0 0 2 8 ]

cap (I _ c _ _ _ _) = c
dur (I _ _ d _ _ _) = d
flav (I _ _ _ f _ _) = f
text (I _ _ _ _ t _) = t
cal (I _ _ _ _ _ c) = c

props = [cap, dur, flav, text]

opts = [[x, y, z, (100 - (x + y + z))] | x <- [0..100]
                                       , y <- [0.. (100 -  x)]
                                       , z <- [0.. (100 - (x+y))]]

cal500 = filter ((==500) . sum .zipWith (\i c -> c*cal i) ingredients)

val coeffs = product $ map (\f -> max 0 . sum $ zipWith (*) coeffs (map f ingredients)) props

part1 = maximum . map val $  opts
part2 = maximum . map val . cal500 $ opts

main = print part1 >> print part2
