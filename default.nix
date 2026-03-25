{ system ? builtins.currentSystem }:

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

  # Import nixpkgs for the requested target system.
  pkgs = import sources.nixpkgs { inherit system; };
in haskellPackages.dailyReporter
