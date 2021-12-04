{ pkgs ? import ./pkgs.nix }: {
  hs = {
    name = "Haskell";
    full = true;
    extension = "hs";
    buildInputs = let
      inherit (pkgs) haskellPackages;
      aoc = haskellPackages.callPackage ./adventofcode/pkg.nix { };
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
  };
  asm = rec {
    name = "ASM";
    full = false;
    extension = "asm";
    buildInputs = with pkgs; [ nasm manpages gdb glibc.dev ];
    buildPhase = ''
      nasm -felf64 run.asm && ld -o run run.o
    '';
  };
  nix = rec {
    name = "Nix";
    full = false;
    extension = "nix";
    runner = pkgs.writeScript "run" ''
      #!${pkgs.bash}/bin/bash
      TMP=$(${pkgs.coreutils}/bin/mktemp -d)
      ${pkgs.nixUnstable}/bin/nix eval \
          --extra-experimental-features nix-command \
          --eval-store $TMP \
          --option store $TMP \
          -f @run@ \
          --raw
      rm -rf $TMP
    '';
    buildInputs = [ ];
    buildPhase = ''
      mkdir -p $out/share/
      cp $src/{run.nix,input.txt} $out/share/

      substitute ${runner} run \
        --subst-var-by run $out/share/run.nix
      chmod +x run
      cat run
    '';
  };
}
