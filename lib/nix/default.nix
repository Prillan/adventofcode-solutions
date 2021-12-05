with builtins;
builtins // rec {
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
  product = foldl' (x: y: x * y) 1;

  readInt = x:
    let v = fromJSON x;
    in if (isInt v) then v else throw "NOT AN INT";
}
