let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  callPackage = pkgs.haskell.packages.ghc912.callPackage;
  theseus = callPackage ./theseus.nix {};
  app = callPackage ./app.nix {inherit theseus;};

  # Fetch the haskell.nix commit we have pinned with Niv
  # haskellNix = import sources.haskellNix {};

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import sources.nixpkgs {};
in app
