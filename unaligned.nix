{ mkDerivation, array, base, bitstring, bytestring, conduit
, containers, extra, hashmap, hspec, HUnit, lib, mtl, QuickCheck
, typenums
}:
mkDerivation {
  pname = "Unaligned";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bitstring bytestring conduit containers extra hashmap
    mtl typenums
  ];
  executableHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base bitstring bytestring conduit hspec HUnit QuickCheck
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
