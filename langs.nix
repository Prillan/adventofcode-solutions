{ pkgs }: {
  hs = {
    name = "Haskell";
    full = true;
    extension = "hs";
    buildInputs =
      let
        inherit (pkgs) haskellPackages;
        aoc = haskellPackages.callPackage ./lib/hs/adventofcode/pkg.nix { };
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
            unordered-containers
            vector
          ];
      in
      [ (haskellPackages.ghcWithPackages ghcPkgs) ];
    buildPhase = ''
      ghc -O2 run.hs
    '';
    shellRunHelp = "ghc -O2 run.hs && time ./run";
  };
  asm = rec {
    name = "ASM";
    full = false;
    extension = "asm";
    buildInputs = with pkgs; [ nasm manpages gdb glibc.dev ];
    buildPhase = ''
      nasm -felf64 run.asm && ld -o run run.o
    '';
    shellRunHelp = "nasm -felf64 run.asm && ld -o run run.o && time ./run < input.txt";
  };
  nix =
    let runner = pkgs.callPackage ./lib/nix/pkg.nix { };
    in
    rec {
      name = "Nix";
      full = false;
      extension = "nix";
      buildInputs = [ pkgs.makeWrapper runner ];
      buildPhase = ''
        mkdir -p $out/share/
        cp $src/{run.nix,input.txt} $out/share/

        makeWrapper ${runner}/bin/nix-eval run \
          --set TARGET $out/share/run.nix
      '';
      shellRunHelp = "time nix-eval ./run.nix";
    };
}
