{ mkDerivation, base, binary, bitstring, bytestring, conduit
, containers, extra, generic-lens, hashmap, hspec, HUnit, lens, lib
, mtl, QuickCheck, typenums, vector
}:
mkDerivation {
  pname = "Unaligned";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bitstring bytestring conduit containers extra generic-lens
    hashmap lens mtl typenums vector
  ];
  executableHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base binary bitstring bytestring conduit hspec HUnit QuickCheck
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
