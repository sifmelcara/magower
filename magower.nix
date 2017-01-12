{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, case-insensitive
      , configurator, directory, filepath, HTTP, http-client-tls
      , http-conduit, http-types, monad-loops, network-uri, stdenv
      }:
  mkDerivation {
      pname = "magower";
      version = "2.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [
        aeson base bytestring case-insensitive configurator directory
        filepath HTTP http-client-tls http-conduit http-types monad-loops
        network-uri
      ];
      description = "magnet thrower";
      license = stdenv.lib.licenses.publicDomain;
  };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
