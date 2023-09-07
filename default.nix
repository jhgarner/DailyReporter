let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  cleff = pkgs.haskell.lib.doJailbreak pkgs.haskellPackages.cleff;
  app = pkgs.haskellPackages.callPackage ./app.nix {inherit cleff;};

  # Fetch the haskell.nix commit we have pinned with Niv
  # haskellNix = import sources.haskellNix {};

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import sources.nixpkgs {};
in app
