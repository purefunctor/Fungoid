let
 pkgs = import <nixpkgs> { };
in
 pkgs.haskellPackages.callPackage ./fungoid.nix { }
