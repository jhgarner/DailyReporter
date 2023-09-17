let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.ghc
    pkgs.haskell-language-server
    pkgs.haskellPackages.fourmolu_0_13_1_0
    # (pkgs.haskell.packages.ghc944.haskell-language-server.override { supportedGhcVersions = [ "944" ]; })
    pkgs.niv
    pkgs.cabal2nix
    pkgs.cabal-install
    pkgs.hpack
    pkgs.gcc
    pkgs.icu
    pkgs.zlib
    pkgs.haskellPackages.hspec-discover
    pkgs.pkg-config
  ];
}
