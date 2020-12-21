{pkgs  ? import <nixpkgs> {}}:
  pkgs.haskellPackages.callPackage ./blogemre.nix {}
