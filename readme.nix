{ pkgs }:
with builtins;
let
  drvs = import ./derivations.nix { inherit pkgs; };
  langs = import ./langs.nix { inherit pkgs; };
  langDrv =
    let f = lang: { inherit (lang) name slug extension full; };
    in
    pkgs.writeText "langs.json" (toJSON (map f (attrValues langs)));
in
pkgs.runCommand "readme"
{
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
