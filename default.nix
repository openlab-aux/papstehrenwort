with import <nixpkgs> {};
let
  hp = haskellPackages.override {
    overrides = with haskell.lib; self: super: {
      papstehrenwort = self.callPackage ./papstehrenwort.nix {};
      papstehrenwortEnv = addBuildDepends self.papstehrenwort [ self.ghcid ];
    };
  };
in
  hp.papstehrenwortEnv
