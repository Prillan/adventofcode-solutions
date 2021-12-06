{ nixpkgs ? import ../../../pkgs.nix }:
nixpkgs.mkShell {
  buildInputs = [
    nixpkgs.cabal2nix
    nixpkgs.haskellPackages.cabal-install
  ] ++ (
    import ./default.nix { inherit nixpkgs; }
  ).env.nativeBuildInputs;
}
