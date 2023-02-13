{ mkDerivation, aeson, aws, base, bytestring, conduit-extra
, containers, exceptions, fast-tagsoup, feed, hashable
, http-conduit, indexed-traversable, lens, lens-aeson, lib
, mime-mail, mime-mail-ses, mtl, recursion-schemes, regex-posix
, resourcet, retry, scalpel, tagsoup, text, time
, unordered-containers, vector
}:
mkDerivation {
  pname = "DailyReporter";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aws base bytestring conduit-extra containers exceptions
    fast-tagsoup feed hashable http-conduit indexed-traversable lens
    lens-aeson mime-mail mime-mail-ses mtl recursion-schemes
    regex-posix resourcet retry scalpel tagsoup text time
    unordered-containers vector
  ];
  executableHaskellDepends = [
    aeson aws base bytestring conduit-extra containers exceptions
    fast-tagsoup feed hashable http-conduit indexed-traversable lens
    lens-aeson mime-mail mime-mail-ses mtl recursion-schemes
    regex-posix resourcet retry scalpel tagsoup text time
    unordered-containers vector
  ];
  testHaskellDepends = [
    aeson aws base bytestring conduit-extra containers exceptions
    fast-tagsoup feed hashable http-conduit indexed-traversable lens
    lens-aeson mime-mail mime-mail-ses mtl recursion-schemes
    regex-posix resourcet retry scalpel tagsoup text time
    unordered-containers vector
  ];
  homepage = "https://github.com/jhgarner/DailyReporter#readme";
  license = lib.licenses.mit;
  mainProgram = "DailyReporter-exe";
}
