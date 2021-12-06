{ mkDerivation, base, containers, lib, split }:
mkDerivation {
  pname = "adventofcode";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers split ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
