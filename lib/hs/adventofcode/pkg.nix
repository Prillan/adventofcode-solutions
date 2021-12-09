{ mkDerivation, base, containers, lib, split, unordered-containers
}:
mkDerivation {
  pname = "adventofcode";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers split unordered-containers
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
