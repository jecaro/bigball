{ nixpkgs ? import <nixpkgs> {} }:
let
  derivations = import ./.;
in
with nixpkgs;
mkShell {
  inputsFrom = [ derivations.bigball.env ];
  buildInputs = [
    ghcid
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.implicit-hie
  ];
}
