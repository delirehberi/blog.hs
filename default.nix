{pkgs  ? import <nixpkgs> {}}:
  pkgs.haskellPackages.callPackage ./come.nix {}
