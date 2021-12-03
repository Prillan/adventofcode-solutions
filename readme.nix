{ pkgs ? import ./pkgs.nix }:
with builtins;
let
  drvs = import ./derivations.nix { inherit pkgs; };
  langs = import ./langs.nix { inherit pkgs; };
  langDrv =
    let f = ext: lang: { inherit (lang) name extension full; };
    in
      pkgs.writeText "langs.json" (toJSON (mapAttrs f langs));
in pkgs.runCommand "readme" {
  template = ./README.base.md;
  generator = ./utils/gen-tables.py;
  challenges = drvs.all;
  buildInputs = drvs.all;
} ''
  mkdir -p $out
  ${pkgs.python3}/bin/python $generator \
      ${langDrv} \
      $template \
      $challenges \
      > $out/README.md
''
