let
  pkgs = import <nixpkgs> { };
  noCheck = pkgs.haskell.lib.dontCheck;
  noDocs = pkgs.haskell.lib.dontHaddock;
  noErrors = pkgs.haskell.lib.doJailbreak;
in
pkgs.mkShell {
  buildInputs = [
    # pkgs.ghc
    # pkgs.haskell-language-server
    # pkgs.haskellPackages.fourmolu
        pkgs.haskell.packages.ghc912.ghc
    (noCheck (noDocs pkgs.haskell.packages.ghc912.haskell-language-server))
    (noCheck (noDocs pkgs.haskell.packages.ghc912.fourmolu))

    # (pkgs.haskell.packages.ghc944.haskell-language-server.override { supportedGhcVersions = [ "944" ]; })
    pkgs.niv
    pkgs.cabal2nix
    pkgs.cabal-install
    pkgs.hpack
    pkgs.gmp
    pkgs.gcc
    pkgs.icu
    pkgs.zlib
    pkgs.haskellPackages.hspec-discover
    pkgs.pkg-config
    pkgs.ncurses
  ];
}
