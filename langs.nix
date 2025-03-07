{ pkgs }: {
  hs = {
    name = "Haskell";
    slug = "haskell";
    url = "https://www.haskell.org/";
    full = true;
    extension = "hs";
    buildInputs =
      let
        inherit (pkgs) haskellPackages;
        aoc = haskellPackages.callPackage ./lib/hs/adventofcode/pkg.nix { };
        ghcPkgs = hpkgs:
          with hpkgs; [
            MonadRandom

            aeson
            aoc
            cryptonite
            fingertree
            hashable
            lens
            matrix
            megaparsec
            multiset
            pipes
            split
            unordered-containers
            vector
          ];
      in
      [ (haskellPackages.ghcWithPackages ghcPkgs) ];
    buildPhase = ''
      ghc -O2 run.hs
    '';
    shellRunHelp = "ghc -O2 run.hs && time ./run";
  };
  asm = rec {
    name = "ASM";
    slug = "asm";
    url = "https://en.wikipedia.org/wiki/X86_assembly_language";
    full = false;
    extension = "asm";
    buildInputs = with pkgs; [ nasm man-pages gdb glibc.dev ];
    buildPhase = ''
      nasm -felf64 run.asm && ld -o run run.o
    '';
    shellRunHelp = "nasm -felf64 run.asm && ld -o run run.o && time ./run < input.txt";
  };
  koka = {
    name = "Koka";
    slug = "koka";
    url = "https://koka-lang.github.io/koka/doc/index.html";
    full = false;
    extension = "kk";
    buildInputs = [ pkgs.koka ];
    # TODO: Add some support for sharing the compilation result of the
    # stdlib, otherwise we recompile it for every single derivation.
    buildPhase = ''
      koka -o run run.kk && chmod +x ./run
    '';
    shellRunHelp = "koka -o run run.kk && time ./run";
  };
  k =
    let runner = pkgs.writeScriptBin "k-runner" ''
      #!${pkgs.bash}/bin/bash
      if [ -z "$TARGET" ]; then
        TARGET="$1"
        shift
      fi
      exec ${pkgs.ngn-k}/bin/k "$TARGET"
      '';
    in
      {
        name = "ngn/k";
        slug = "k";
        url = "https://xpqz.github.io/kbook/Introduction.html";
        full = false;
        extension = "k";
        buildInputs = [ pkgs.makeWrapper pkgs.ngn-k pkgs.rlwrap runner ];
        buildPhase = ''
          mkdir -p $out/share/
          cp $src/run.k $out/share/

          makeWrapper ${runner}/bin/k-runner run \
            --set TARGET $out/share/run.k
        '';
        shellRunHelp = ''
          k-runner run.k
          # Dev:
          # rlwrap k-repl
        '';
      };
  nix =
    let runner = pkgs.callPackage ./lib/nix/pkg.nix { };
    in
    rec {
      name = "Nix";
      slug = "nix";
      url = "https://nixos.org/";
      full = false;
      extension = "nix";
      buildInputs = [ pkgs.makeWrapper runner ];
      buildPhase = ''
        mkdir -p $out/share/
        cp $src/run.nix $out/share/

        makeWrapper ${runner}/bin/nix-eval run \
          --set TARGET $out/share/run.nix
      '';
      shellRunHelp = "time nix-eval ./run.nix";
    };
}
