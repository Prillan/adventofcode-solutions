{ pkgs, ... }:
let lib = ./.;
    runner = pkgs.writeScriptBin "nix-eval" ''
      #!${pkgs.bash}/bin/bash
      TMP=$(${pkgs.coreutils}/bin/mktemp -d)
      if [ -z "$TARGET" ]; then
        TARGET="$1"
        shift
      fi
      INPUT="$1"
      ${pkgs.nixUnstable}/bin/nix eval \
        --extra-experimental-features nix-command \
        --eval-store $TMP \
        --option store $TMP \
        --apply 'f: f { lib = import ${lib}; inputFile = "'"$INPUT"'"; }' \
        -f "$TARGET" \
        --raw
      rm -rf $TMP
  '';
in runner
