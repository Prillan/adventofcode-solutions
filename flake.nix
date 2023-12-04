{
  description = "Advent of Code";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        readme = import ./readme.nix { inherit pkgs; };
        copyReadme = pkgs.writeScriptBin "copy-readme" ''
          cp ${readme}/README.md .
          chmod u+w README.md
        '';
        derivations = import ./derivations.nix { inherit pkgs; };
        langs = import ./langs.nix { inherit pkgs; };

        aoc = pkgs.callPackage ./utils/aoc/aoc.nix {};
      in
      {
        apps = {
          aoc = {
            type = "app";
            program = "${aoc}/bin/aoc";
          };
          readme = {
            type = "app";
            program = "${copyReadme}/bin/copy-readme";
          };
        };
        lib = {
          inherit langs;
        };
        packages = {
          aoc = aoc;
          days = derivations;
        };
        devShells =
          let langShell = lang: { name, buildInputs, shellRunHelp, extension, ... }: pkgs.mkShell {
            inherit buildInputs;
            shellHook = ''
              echo '------------------------'
              echo 'Shell for: ${name}'
              echo 'Compile and run with: '
              echo '  ${shellRunHelp}'
              echo '------------------------'
            '';
          };
          in
          {
            langs = builtins.mapAttrs langShell langs;
            libs.hs = pkgs.mkShell {
              buildInputs = [
                pkgs.cabal2nix
                pkgs.haskellPackages.cabal-install
                pkgs.ghc
              ];
            };
          };
      }
    );
}
