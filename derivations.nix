{ pkgs ? import ./pkgs.nix }:
let
  days = import ./days.nix;
  matches = regex: str: builtins.match regex str != null;
  const = a: b: a;
  langs = [
    rec {
      name = "Haskell";
      extension = "hs";
      buildInputs = let
        inherit (pkgs) haskellPackages;
        aoc = haskellPackages.callPackage ./adventofcode/default.nix { };
        ghcPkgs = hpkgs:
          with hpkgs; [
            MonadRandom
            aeson
            aoc
            cryptonite
            fingertree
            lens
            megaparsec
            multiset
            pipes
            split
            vector
          ];
      in [ (haskellPackages.ghcWithPackages ghcPkgs) ];
      buildPhase = ''
        ghc -O2 run.hs
      '';
    }
    rec {
      name = "Assembler";
      extension = "asm";
      buildInputs = with pkgs; [ nasm manpages gdb glibc.dev ];
      buildPhase = ''
        nasm -felf64 run.asm && ld -o run run.o
      '';
    }
  ];
  drv = y: d: lang:
    with lang;
    pkgs.stdenv.mkDerivation {
      name = "aoc-${name}-${toString y}-day${toString d}";
      src = builtins.filterSource (path: type:
        (matches ".*.${extension}" path) || (matches ".*.txt" path))
        (dayPath y d);
      inherit buildInputs;
      buildPhase = if builtins.pathExists (dayLangSkipPath y d lang) then
        "touch skip"
      else
        buildPhase;
      installPhase = ''
        mkdir -p $out
        if ! [ -e skip ]; then
          mkdir -p $out/bin
          cp run $out/bin
        fi
        cp status $out
      '';
      doCheck = true;
      checkPhase = ''
        if ! [ -e expected.txt ] || [ -e skip ]; then
          echo '?' > status
        else
          touch input.txt
          time ./run input.txt < input.txt > result.txt
          if diff -B -Z result.txt expected.txt; then
            echo 'Got the expected results!'
            echo 'OK' > status
          else
            echo 'Oh-oh, tests failed!'
            echo Got
            cat result.txt
            echo Expected
            cat expected.txt
            exit 1
          fi
        fi
      '';
    };
  dayPath = y: d: ./. + "/${y}/day${toString d}";
  dayLangPath = y: d: lang: dayPath y d + "/run.${lang.extension}";
  dayLangSkipPath = y: d: lang: dayPath y d + "/.skip.${lang.extension}";
  dayLangs = y: d:
    let hasCodeFor = lang: builtins.pathExists (dayLangPath y d lang);
    in builtins.filter hasCodeFor langs;
  dayDrvs = y: d:
    builtins.listToAttrs (map (lang: {
      name = lang.extension;
      value = drv y d lang;
    }) (dayLangs y d));
  drvs = builtins.listToAttrs (map (y: {
    name = "year${y}";
    value = builtins.listToAttrs (map (d: {
      name = "day${toString d}";
      value = dayDrvs y d;
    }) days.${y});
  }) (builtins.attrNames days));
  flat = with builtins;
    let flatten1 = xs: concatLists (map attrValues xs);
    in flatten1 (flatten1 (attrValues drvs));
in drvs // { all = flat; }
