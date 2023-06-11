let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.packages.ghc944.ghc
    (pkgs.lib.pipe pkgs.haskell.packages.ghc944.haskell-language-server [pkgs.haskell.lib.compose.dontHaddock pkgs.haskell.lib.compose.dontCheck])
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
