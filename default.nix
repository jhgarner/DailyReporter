let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix {};

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    haskellNix.sources.nixpkgs-2009
    haskellNix.nixpkgsArgs;
  dailyReporter = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "DailyReporter";
      src = ./.;
    };
  };
in pkgs.linkFarm "DailyReporter-with-config" [
  { "name" = "templates"; "path" = ./templates; }
  { "name" = "parsers"; "path" = ./parsers; }
  { "name" = "dailyReporter"; "path" = dailyReporter.DailyReporter.components.exes.DailyReporter-exe; }
]
