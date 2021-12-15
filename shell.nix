let
  defaultExtraDeps = {
    hs = hpkgs: [];
    asm = [];
  };
in { lang ? "hs", nixpkgs ? import ./pkgs.nix, extraDeps ? defaultExtraDeps }:
  let
  envs = {
    hs = let
      haskellPackages = nixpkgs.haskellPackages.override {
        overrides = self: super: {
          aoc = super.callPackage ./lib/hs/adventofcode/pkg.nix { };
        };
      };
      pkgs = hpkgs: [
        hpkgs.megaparsec
        hpkgs.split
        hpkgs.MonadRandom
        hpkgs.vector
        hpkgs.aoc
        hpkgs.fingertree
        hpkgs.unordered-containers
      ] ++ (extraDeps.hs hpkgs);
    in [ (haskellPackages.ghcWithPackages pkgs) ];
    asm = with nixpkgs; [ nasm manpages gdb glibc.dev ];
    nix = [ (nixpkgs.callPackage ./lib/nix/pkg.nix {}) ];
  };
in nixpkgs.mkShell { buildInputs = envs."${lang}"; }
