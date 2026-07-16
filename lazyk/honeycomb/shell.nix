{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.ghc.withPackages (P: [ P.recursion-schemes ]);
in
pkgs.mkShell {
  buildInputs = [ ghc ];
}
