{ mkDerivation, base, bitstring, bytestring, containers, hspec
, HUnit, lib, mtl, QuickCheck, typenums
}:
mkDerivation {
  pname = "Unaligned";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bitstring bytestring containers mtl typenums
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bitstring bytestring hspec HUnit QuickCheck
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
