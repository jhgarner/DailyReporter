let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.ghc
    pkgs.haskell-language-server
    pkgs.niv
    pkgs.cabal-install
    pkgs.hpack
    pkgs.gcc
    pkgs.icu
    pkgs.zlib
  ];
}
