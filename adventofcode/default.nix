{ nixpkgs ? import ../pkgs.nix }:
nixpkgs.pkgs.haskellPackages.callPackage ./pkg.nix { }
