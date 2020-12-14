let
  commit = "b94726217f7cdc02ddf277b65553762d520da196";
  archive = "https://github.com/NixOS/nixpkgs/archive/" + commit + ".tar.gz";
in { nixpkgs ? import (fetchTarball archive) { } }:
let
  inherit (nixpkgs) haskellPackages;
  aoc = haskellPackages.callPackage ./adventofcode/default.nix { };
in nixpkgs.mkShell {
  buildInputs =
    [ (haskellPackages.ghcWithPackages (hpkgs: [ hpkgs.split aoc ])) ];
}
