let 
  compiler = "ghc8107";
  overlays = [(
    final: prev:

      rec {
        haskell = prev.haskell // { 
          packages = prev.haskell.packages // {
            "${compiler}" = prev.haskell.packages."${compiler}".override 
            {
              overrides = haskellPackagesNew: haskellPackagesOld:
              rec  {
                lzw = haskellPackagesNew.callPackage ./LZW.nix { };
              };
            };
          };
        };
      }
    )
  ];
in
  {pkgs ? import <nixpkgs> {inherit overlays;}}:
  {
    lzw = pkgs.haskell.packages."${compiler}".lzw;
  }
