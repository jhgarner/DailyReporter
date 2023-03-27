{ mkDerivation, aeson, base, bytestring, cleff, cleff-plugin
, conduit-extra, containers, deriving-aeson, exceptions
, fast-tagsoup, feed, hashable, hspec, hspec-discover, http-conduit
, indexed-traversable, lens, lens-aeson, lib, modern-uri, mtl, PyF
, recursion-schemes, regex-posix, req, resourcet, retry, scalpel
, tagsoup, text, time, unliftio, unordered-containers, vector
}:
mkDerivation {
  pname = "DailyReporter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cleff cleff-plugin conduit-extra containers
    deriving-aeson exceptions fast-tagsoup feed hashable http-conduit
    indexed-traversable lens lens-aeson modern-uri mtl PyF
    recursion-schemes regex-posix req resourcet retry scalpel tagsoup
    text time unliftio unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson base bytestring cleff cleff-plugin conduit-extra containers
    deriving-aeson exceptions fast-tagsoup feed hashable http-conduit
    indexed-traversable lens lens-aeson modern-uri mtl PyF
    recursion-schemes regex-posix req resourcet retry scalpel tagsoup
    text time unliftio unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring cleff cleff-plugin conduit-extra containers
    deriving-aeson exceptions fast-tagsoup feed hashable hspec
    http-conduit indexed-traversable lens lens-aeson modern-uri mtl PyF
    recursion-schemes regex-posix req resourcet retry scalpel tagsoup
    text time unliftio unordered-containers vector
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  homepage = "https://github.com/jhgarner/DailyReporter#readme";
  license = lib.licenses.mit;
  mainProgram = "DailyReporter-exe";
}
