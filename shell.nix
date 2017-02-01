{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, labels, scientific, text, stdenv, vector-algorithms, clock, mtl, wai, warp, http-streams, weigh, retry, safe-exceptions, criterion }:
      mkDerivation {
        pname = "prometheus-effect";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base labels scientific text vector-algorithms clock mtl wai warp http-streams retry safe-exceptions criterion ];
        testHaskellDepends = [ weigh ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
