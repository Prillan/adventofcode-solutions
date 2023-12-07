{ pkgs }:
let
  days = import ./days.nix;
  matches = regex: str: builtins.match regex str != null;
  const = a: b: a;
  langs = import ./langs.nix { inherit pkgs; };
  drv = y: d: lang:
    let
      inherit (lang) name slug extension buildInputs buildPhase;
      challengeMeta = {
        year = builtins.fromJSON y;
        day = d;
        lang = {
          inherit name extension slug;
        };
      };
      binary = pkgs.stdenv.mkDerivation {
        __contentAddressed = true;
        name = "aoc-binary-${name}-year${toString y}-day${toString d}";
        src = builtins.filterSource
          (path: type: matches ".*.${extension}" path)
          (dayPath y d);
        buildInputs = buildInputs;
        buildPhase =
          if builtins.pathExists (dayLangSkipPath y d lang) then
            "touch skip"
          else
            buildPhase;
        installPhase = ''
          mkdir -p $out
          if [ -e skip ]; then
            touch $out/skip
          else
            mkdir -p $out/bin
            cp run $out/bin
          fi
        '';
      };
    in
      pkgs.stdenv.mkDerivation {
        name = "aoc-${name}-year${toString y}-day${toString d}";
        src = builtins.filterSource
          (path: type: matches ".*.txt" path)
          (dayPath y d);
        buildInputs = [ binary ] ++ [ pkgs.jq ];
        challengeMeta = builtins.toJSON challengeMeta;
        passAsFile = [ "challengeMeta" ];
        doCheck = true;
        checkPhase = ''
          if ! [ -e expected.txt ] || [ -e ${binary}/skip ]; then
            echo '?' > status
          else
            touch input.txt
            # Note: absolute path needed for the nix runner!!!
            time run $(pwd)/input.txt < input.txt > result.txt
            if diff -B -Z result.txt expected.txt; then
              echo 'Got the expected results!'
              echo 'G' > status
            else
              echo 'Oh-oh, tests failed!'
              echo Got
              cat result.txt
              echo Expected
              cat expected.txt
              echo 'B' > status
            fi
          fi
        '';
        installPhase = ''
          mkdir -p $out
          jq ".status = \"$(cat status)\" | .binaryDrv = \"${binary}\"" \
            < $challengeMetaPath \
            > $out/meta.json
        '';
      };
  dayPath = y: d: ./. + "/${y}/day${toString d}";
  dayLangPath = y: d: lang: dayPath y d + "/run.${lang.extension}";
  dayLangSkipPath = y: d: lang: dayPath y d + "/.skip.${lang.extension}";
  dayLangs = y: d:
    let hasCodeFor = lang: builtins.pathExists (dayLangPath y d lang);
    in builtins.filter hasCodeFor (builtins.attrValues langs);
  dayDrvs = y: d:
    builtins.listToAttrs (map
      (lang: {
        name = lang.extension;
        value = drv y d lang;
      })
      (dayLangs y d));
  drvs = builtins.listToAttrs (map
    (y: {
      name = "year${y}";
      value = builtins.listToAttrs (map
        (d: {
          name = "day${toString d}";
          value = dayDrvs y d;
        })
        days.${y});
    })
    (builtins.attrNames days));
  flat = with builtins;
    let flatten1 = xs: concatLists (map attrValues xs);
    in flatten1 (flatten1 (attrValues drvs));
in
drvs // {
  all = flat;
  tree = pkgs.linkFarm "tree" (map (drv: {name=drv.name; path=drv;}) flat);
}
