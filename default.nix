{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
         then pkgs.haskellPackages
         else pkgs.haskell.packages.${compiler};
in
  {
    inductive-graphs = haskellPackages.callPackage ./inductive-graphs.nix {};
  }
