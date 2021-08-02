{ nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc8104" }:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  doJailbreak = nixpkgs.pkgs.haskell.lib.doJailbreak;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      gedcom = self.callCabal2nix "gedcom" (gitignore ./.) {};
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.gedcom
    ];
    buildInputs = [
      nixpkgs.haskellPackages.cabal-install
      nixpkgs.wget
      nixpkgs.haskellPackages.ghcid
      nixpkgs.haskellPackages.ormolu
    ];
    withHoogle = false;
  };
  exe = nixpkgs.haskell.lib.justStaticExecutables (myHaskellPackages.gedcom);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  gedcom = myHaskellPackages.gedcom;
}
