{ pkgs ? import ./pkgs.nix }: {
  hs = {
    name = "Haskell";
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
    extension = "asm";
    buildInputs = with pkgs; [ nasm manpages gdb glibc.dev ];
    buildPhase = ''
      nasm -felf64 run.asm && ld -o run run.o
    '';
  };
}
