{ mkDerivation, base, containers, fingertree, hashable, lib
, megaparsec, split, unordered-containers
}:
mkDerivation {
  pname = "adventofcode";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers fingertree hashable megaparsec split
    unordered-containers
  ];
  license = "unknown";
}
