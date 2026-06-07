{ mkDerivation, aeson, attoparsec, attoparsec-aeson, base
, base64-bytestring, bytestring, containers, directory, exceptions
, filepath, http-client, http-types, lib, microlens, microlens-th
, monad-logger, mtl, network, network-uri, optparse-applicative
, random, retry, safe-exceptions, sandwich, sandwich-contexts
, scientific, stm, string-interpolate, text, time, unliftio
, unliftio-core, unordered-containers, wai-app-static, warp
, websockets, zip-archive
}:
mkDerivation {
  pname = "webdriver";
  version = "0.14.0.0";
  sha256 = "080fbb6988369e238c6eaf5e7bed57e1e79ece42d07340a633f6a81a0485176b";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec attoparsec-aeson base base64-bytestring bytestring
    containers directory exceptions filepath http-client http-types
    microlens-th monad-logger mtl network network-uri random retry
    safe-exceptions scientific stm string-interpolate text time
    unliftio unliftio-core unordered-containers websockets zip-archive
  ];
  executableHaskellDepends = [
    aeson base bytestring exceptions http-client http-types
    monad-logger mtl string-interpolate text unliftio unliftio-core
  ];
  testHaskellDepends = [
    aeson base bytestring containers exceptions filepath http-client
    http-types microlens monad-logger mtl network network-uri
    optparse-applicative retry safe-exceptions sandwich
    sandwich-contexts string-interpolate text unliftio unliftio-core
    wai-app-static warp
  ];
  testToolDepends = [ sandwich ];
  homepage = "https://github.com/haskell-webdriver/haskell-webdriver#readme";
  description = "a Haskell client for the Selenium WebDriver protocol";
  license = lib.licenses.bsd3;
  mainProgram = "demo";
}
