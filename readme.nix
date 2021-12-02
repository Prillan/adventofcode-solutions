{ pkgs ? import ./pkgs.nix }:
let
  drvs = import ./derivations.nix { inherit pkgs; };
in pkgs.runCommand "readme" {
  template = ./README.base.md;
  generator = ./utils/gen-tables.py;
  buildInputs = drvs.all;
} ''
  mkdir -p $out
  ${pkgs.python3}/bin/python $generator $template $buildInputs > $out/README.md
''
