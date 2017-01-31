{ mkDerivation, base, bytestring, clock, containers, ghc-prim
, http-streams, http-types, io-streams, labels, mtl, random, retry
, safe-exceptions, scientific, stdenv, text, transformers, vector
, vector-algorithms, wai, warp, weigh
}:
mkDerivation {
  pname = "prometheus-effect";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring clock containers ghc-prim http-streams http-types
    io-streams labels mtl retry safe-exceptions scientific text
    transformers vector vector-algorithms wai
  ];
  executableHaskellDepends = [ base http-types random wai warp ];
  testHaskellDepends = [ base weigh ];
  license = stdenv.lib.licenses.bsd3;
}
