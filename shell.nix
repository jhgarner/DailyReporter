let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.haskell-language-server
    pkgs.niv
  ];
}
