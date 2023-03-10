{ mkDerivation, aeson, base, bytestring, conduit-extra, containers
, deriving-aeson, exceptions, fast-tagsoup, feed, hashable
, http-conduit, indexed-traversable, lens, lens-aeson, lib, mtl
, PyF, recursion-schemes, regex-posix, req, resourcet, retry
, scalpel, tagsoup, text, time, unordered-containers, vector
}:
mkDerivation {
  pname = "DailyReporter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring conduit-extra containers deriving-aeson
    exceptions fast-tagsoup feed hashable http-conduit
    indexed-traversable lens lens-aeson mtl PyF recursion-schemes
    regex-posix req resourcet retry scalpel tagsoup text time
    unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson base bytestring conduit-extra containers deriving-aeson
    exceptions fast-tagsoup feed hashable http-conduit
    indexed-traversable lens lens-aeson mtl PyF recursion-schemes
    regex-posix req resourcet retry scalpel tagsoup text time
    unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring conduit-extra containers deriving-aeson
    exceptions fast-tagsoup feed hashable http-conduit
    indexed-traversable lens lens-aeson mtl PyF recursion-schemes
    regex-posix req resourcet retry scalpel tagsoup text time
    unordered-containers vector
  ];
  homepage = "https://github.com/jhgarner/DailyReporter#readme";
  license = lib.licenses.mit;
  mainProgram = "DailyReporter-exe";
}
