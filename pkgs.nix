let
  commit = "b94726217f7cdc02ddf277b65553762d520da196";
  archive = "https://github.com/NixOS/nixpkgs/archive/" + commit + ".tar.gz";
in import (fetchTarball archive) { }
