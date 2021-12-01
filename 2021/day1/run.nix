with builtins;

let
  lines = str: filter (x: isString x && x != "") (split "\n" str);
  unlines = concatStringsSep "\n";
  drop = n: x: if n == 0 || x == [] then x else drop (n - 1) (tail x);
  take = n: x: if n == 0 || x == [] then [] else [ (head x) ] ++ take (n - 1) (tail x);
  scanl = f: start: xs:
    let g = { last, acc }: x:
          let next = f last x;
          in {
            acc = acc ++ [ next ];
            last = next;
          };
    in (foldl' g { last = start; acc = [start]; } xs).acc;
  sum = foldl' (x: y: x + y) 0;

  parse = fromJSON; # Super-hacky way of reading ints
  parseAll = x: map parse (lines x);

  input = parseAll (readFile ./input.txt);

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
