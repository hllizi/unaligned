let
  compiler = "ghc8107";
  overlays = [
    (
      final: prev:

        rec {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              "${compiler}" = prev.haskell.packages."${compiler}".override
                {
                  overrides = haskellPackagesNew: haskellPackagesOld:
                    rec  {
                      unaligned = haskellPackagesNew.callPackage ./unaligned.nix { };
                      Memoization = haskellPackagesNew.callPackage ../Memoization/memoization.nix { };
                    };
                };
            };
          };
        }
    )
  ];
in
{ pkgs ? import <nixpkgs> { inherit overlays; } }:
{
  unaligned = pkgs.haskell.packages."${compiler}".unaligned;
}
