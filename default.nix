let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          bigball =
            haskellPackagesNew.callPackage ./bigball.nix { };

          path =
            haskellPackagesNew.callPackage ./path.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { bigball = pkgs.haskellPackages.bigball;
  }
