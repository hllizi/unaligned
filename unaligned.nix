{ mkDerivation, array, base, bitstring, bytestring, containers
, extra, hspec, HUnit, lib, mtl, QuickCheck, typenums
}:
mkDerivation {
  pname = "Unaligned";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bitstring bytestring containers extra mtl typenums
  ];
  executableHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base bitstring bytestring hspec HUnit QuickCheck
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
