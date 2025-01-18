{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  # description = "A Hello World in Haskell with a dependency and a devShell";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      });
    in
    {
      overlays.default = (final: prev: {
        bigball = final.haskellPackages.callCabal2nix "bigball" ./. { };
      });

      packages = forAllSystems (system: {
        bigball = nixpkgsFor.${system}.bigball;
        default = self.packages.${system}.bigball;
      });

      checks = self.packages;

      devShells = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in
        {
          default = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.bigball ];
            withHoogle = true;
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
            ];
          };
        }
      );
    };
}
