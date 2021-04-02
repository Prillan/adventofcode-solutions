{ pkgs ? import ./pkgs.nix }:
let
  days = import ./days.nix;
  matches = regex: str: builtins.match regex str != null;
  const = a: b: a;
  langs = [
    rec {
      name = "Haskell";
      extension = "hs";
      buildInputs = extraDeps:
        assert builtins.isAttrs extraDeps;
        let
          inherit (pkgs) haskellPackages;
          extras = extraDeps.hs or (const [ ]);
          aoc = haskellPackages.callPackage ./adventofcode/default.nix { };
          ghcPkgs = hpkgs:
            with hpkgs;
            [ megaparsec split MonadRandom vector aoc ] ++ extras hpkgs;
        in [ (haskellPackages.ghcWithPackages ghcPkgs) ];
      buildPhase = ''
        ghc -O2 run.hs
      '';
    }
    rec {
      name = "Assembler";
      extension = "asm";
      buildInputs = extraDeps:
        with pkgs;
        [ nasm manpages gdb glibc.dev ] ++ (extraDeps.asm or [ ]);
      buildPhase = ''
        nasm -felf64 run.asm && ld -o run run.o
      '';
    }
  ];
  drv = y: d: lang:
    with lang;
    pkgs.stdenv.mkDerivation {
      name = "aoc-${name}-${toString y}-day${toString d}";
      buildInputs = [  ] ++ buildInputs (dayDeps y d);
      src = builtins.filterSource (path: type:
        (matches ".*.${extension}" path) || (matches ".*.txt" path))
        (dayPath y d);
      inherit buildPhase;
      installPhase = ''
        mkdir -p $out/bin
        cp run $out/bin
      '';
      doCheck = true;
      checkPhase = ''
        touch input.txt expected.txt
        time ./run input.txt < input.txt > result.txt
        if diff -B -Z result.txt expected.txt; then
          echo 'Got the expected results!'
        else
          echo 'Oh-oh, tests failed!'
          echo Got
          cat result.txt
          echo Expected
          cat expected.txt
          exit 1
        fi
      '';
    };
  dayPath = y: d: ./. + "/${y}/day${toString d}";
  dayDeps = y: d:
    let p = dayPath y d + "/extra.nix";
    in if builtins.pathExists p then import p else { };
  dayLangPath = y: d: lang: dayPath y d + "/run.${lang.extension}";
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
in drvs
