{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, clay, cron, hashable, hsparql
      , mime-mail, rdf4h, servant, servant-blaze, servant-server
      , smtp-mail, stdenv
      }:
      let
      hsparql-new = with pkgs.haskell.lib;
        dontCheck (hsparql.override { rdf4h = dontCheck rdf4h; });
      in
      mkDerivation {
        pname = "papstehrenwort";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base clay cron hashable hsparql-new mime-mail servant
          servant-blaze servant-server smtp-mail
        ];
        description = "Commit yourself to a task. Like a pope.";
        license = stdenv.lib.licenses.agpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
