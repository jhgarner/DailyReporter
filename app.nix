{ mkDerivation, aeson, base, bytestring, cleff, deriving-aeson
, fast-tagsoup, feed, generic-data, hashable, hspec, hspec-discover
, http-api-data, indexed-traversable, lib, modern-uri, PyF
, recursion-schemes, req, scalpel-core, tagsoup, text, time
, unliftio
}:
mkDerivation {
  pname = "DailyReporter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cleff deriving-aeson fast-tagsoup feed
    generic-data hashable http-api-data indexed-traversable modern-uri
    PyF recursion-schemes req scalpel-core tagsoup text time unliftio
  ];
  executableHaskellDepends = [
    aeson base bytestring cleff deriving-aeson fast-tagsoup feed
    generic-data hashable http-api-data indexed-traversable modern-uri
    PyF recursion-schemes req scalpel-core tagsoup text time unliftio
  ];
  testHaskellDepends = [
    aeson base bytestring cleff deriving-aeson fast-tagsoup feed
    generic-data hashable hspec http-api-data indexed-traversable
    modern-uri PyF recursion-schemes req scalpel-core tagsoup text time
    unliftio
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  homepage = "https://github.com/jhgarner/DailyReporter#readme";
  license = lib.licenses.mit;
  mainProgram = "DailyReporter-exe";
}
