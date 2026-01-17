let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  
  haskellPackages = pkgs.haskell.packages.ghc912.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        doCheck = false;
        doHaddock = false;
      });
      theseus = super.callPackage ./theseus.nix {};
      dailyReporter = super.callPackage ./app.nix {};
    };
  };

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import sources.nixpkgs { };
in haskellPackages.dailyReporter
