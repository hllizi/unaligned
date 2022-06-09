let lzwOverlay = final: prev:
{

  haskellPackages = prev.haskellPackages.override 
  rec   {
    overrides = haskellPackagesNew: haskellPackagesOld:
    {
      lzw = haskellPackagesNew.callPackage ./LZW.nix { };
    };
  };
};
in
  {pkgs ? import <nixpkgs> {overlays = [
    lzwOverlay
  ];}}:
  {
    lzw = pkgs.haskellPackages.lzw;
  }
