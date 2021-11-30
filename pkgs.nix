let
  commit = "96b4157790fc96e70d6e6c115e3f34bba7be490f";
  archive = "https://github.com/NixOS/nixpkgs/archive/" + commit + ".tar.gz";
in import (fetchTarball archive) { }
