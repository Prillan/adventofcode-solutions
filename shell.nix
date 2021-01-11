let
  commit = "b94726217f7cdc02ddf277b65553762d520da196";
  archive = "https://github.com/NixOS/nixpkgs/archive/" + commit + ".tar.gz";

  defaultExtraDeps = {
    hs = hpkgs: [];
    asm = [];
  };
in { lang ? "hs", nixpkgs ? import (fetchTarball archive) { }, extraDeps ? defaultExtraDeps }:
let
  envs = {
    hs = let
      inherit (nixpkgs) haskellPackages;
      aoc = haskellPackages.callPackage ./adventofcode/default.nix { };
      pkgs = hpkgs: [
        hpkgs.megaparsec
        hpkgs.split
        hpkgs.MonadRandom
        hpkgs.vector
        aoc
      ] ++ (extraDeps.hs hpkgs);
    in [ (haskellPackages.ghcWithPackages pkgs) ];
    asm = with nixpkgs; [ nasm manpages gdb glibc.dev ];
  };
in nixpkgs.mkShell { buildInputs = envs."${lang}"; }
