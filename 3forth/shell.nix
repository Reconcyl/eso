{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = let P = pkgs; in [
    P.janet
    P.jpm
  ];
}
