with import <nixpkgs> {};
let
  hp = haskellPackages.override {
    overrides = with haskell.lib; self: super: with self;
    let
      libraryHaskellDepends = [
        aeson base blaze-html blaze-markup cron errors http-api-data
        http-media lens mime-mail persistent persistent-template protolude
        servant servant-blaze servant-server time url wai warp
      ];
      executableHaskellDepends = [ protolude warp ];
      testHaskellDepends = [
        base protolude smallcheck tasty tasty-hunit tasty-smallcheck time
      ];
      allHaskellDepends = libraryHaskellDepends ++ executableHaskellDepends
                         ++ testHaskellDepends;

    in {
      papstehrenwort =
        let hoogle = hoogleLocal { packages = allHaskellDepends; };
        in self.mkDerivation {
          pname = "papstehrenwort";
          version = "0.1.0.0";
          src = ./.;
          isLibrary = true;
          isExecutable = true;
          description = "Commit yourself to a task. Like a pope.";
          license = stdenv.lib.licenses.agpl3;

          inherit libraryHaskellDepends executableHaskellDepends testHaskellDepends;
          buildTools = [ ghcid hoogle ];
      };
    };
  };
in
  hp.papstehrenwort
