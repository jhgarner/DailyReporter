let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  app = pkgs.haskellPackages.callPackage ./app.nix {};

  # Fetch the haskell.nix commit we have pinned with Niv
  # haskellNix = import sources.haskellNix {};

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import sources.nixpkgs {};
in app
