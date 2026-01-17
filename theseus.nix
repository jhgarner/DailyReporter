{ mkDerivation, base, fetchgit, hspec, lib, mtl, QuickCheck
, temporary
}:
mkDerivation {
  pname = "theseus";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/jhgarner/Theseus.git";
    sha256 = "0pdy5l8d6ykjv9n1pqwzjfap3y10bkdl9cz0g2qh1b4smlajj29j";
    rev = "7a0f203bc9d5c087913a6fab3a11c1cf7da585db";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base mtl ];
  testHaskellDepends = [ base hspec mtl QuickCheck temporary ];
  license = lib.licenses.bsd3;
}
