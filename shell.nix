{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) haskellPackages;
  aoc = haskellPackages.callPackage ./adventofcode/default.nix {  };
in nixpkgs.mkShell {
  buildInputs = [ (haskellPackages.ghcWithPackages (hpkgs: [hpkgs.split aoc])) ];
}
