{ mkDerivation, base, fetchgit, hspec, lib, mtl, QuickCheck
, temporary
}:
mkDerivation {
  pname = "theseus";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/jhgarner/Theseus.git";
    sha256 = "sha256-HooGluKReEwVGB+JcV5zzM9ACku94UT8ljuB7q2vMJE=";
    rev = "0d42f255e122f41ac71e36f95b1093ff8158b8c1";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base mtl ];
  testHaskellDepends = [ base hspec mtl QuickCheck temporary ];
  license = lib.licenses.bsd3;
}
