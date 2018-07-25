{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
(import ./default.nix { inherit nixpkgs compiler; }).inductive-graphs.env
