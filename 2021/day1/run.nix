{ lib ? import ../../lib/nix, inputFile }:
with lib;

let
  parse = readInt;
  parseAll = x: map parse (lines x);

  input = parseAll (readFile inputFile);

  countIncreasing = xs:
    let f = { count ? 0, last ? null }: current: {
          last = current;
          count = count + (if last != null && current > last then 1 else 0);
        };
    in (foldl' f { } xs).count;

  part1 = countIncreasing;

  windowOf3 = xs:
    let f = acc: cur: tail (acc ++ [cur]);
        initial = take 3 xs;
        rest = drop 3 xs;
    in filter (x: length x == 3) (scanl f initial rest);

  part2 = xs: countIncreasing (map sum (windowOf3 xs));
in unlines [
  (toString (part1 input))
  (toString (part2 input))
  ""
]
