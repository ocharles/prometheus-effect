{ mkDerivation, base, bytestring, clock, hashable, http-streams
, http-types, io-streams, mtl, random, retry, safe-exceptions
, stdenv, streaming, text, transformers, unordered-containers
, vector, wai, warp, weigh
}:
mkDerivation {
  pname = "prometheus-effect";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring clock hashable http-streams http-types io-streams
    mtl retry safe-exceptions streaming text transformers
    unordered-containers vector wai
  ];
  executableHaskellDepends = [
    base http-types random text wai warp
  ];
  testHaskellDepends = [ base text weigh ];
  license = stdenv.lib.licenses.bsd3;
}
