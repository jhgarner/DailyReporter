{ mkDerivation, aeson, base, bytestring, cleff, conduit-extra
, containers, deriving-aeson, fast-tagsoup, feed, file-embed
, hashable, hspec, hspec-discover, http-api-data, http-conduit
, indexed-traversable, lens, lens-aeson, lib, modern-uri, PyF
, recursion-schemes, req, scalpel, tagsoup, text, time, unliftio
, vector
}:
mkDerivation {
  pname = "DailyReporter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cleff conduit-extra containers deriving-aeson
    fast-tagsoup feed file-embed hashable http-api-data http-conduit
    indexed-traversable lens lens-aeson modern-uri PyF
    recursion-schemes req scalpel tagsoup text time unliftio vector
  ];
  executableHaskellDepends = [
    aeson base bytestring cleff conduit-extra containers deriving-aeson
    fast-tagsoup feed file-embed hashable http-api-data http-conduit
    indexed-traversable lens lens-aeson modern-uri PyF
    recursion-schemes req scalpel tagsoup text time unliftio vector
  ];
  testHaskellDepends = [
    aeson base bytestring cleff conduit-extra containers deriving-aeson
    fast-tagsoup feed file-embed hashable hspec http-api-data
    http-conduit indexed-traversable lens lens-aeson modern-uri PyF
    recursion-schemes req scalpel tagsoup text time unliftio vector
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  jailbreak = true;
  doCheck = false;
  homepage = "https://github.com/jhgarner/DailyReporter#readme";
  license = lib.licenses.mit;
  mainProgram = "DailyReporter-exe";
}
