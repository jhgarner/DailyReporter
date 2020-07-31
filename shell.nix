let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.haskellPackages.ghcide
    pkgs.docker
  ];
}
