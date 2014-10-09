{ pkgs ? import <nixpkgs> {}, haskellPackages ? pkgs.haskellPackages }:

let 
  tmpHaskellPkgs= haskellPackages.override {
        extension = self: super: {
          blankCanvas = self.callPackage /home/bergey/code/nixHaskellVersioned/blank-canvas/0.5.nix {};
          kansasComet = self.callPackage /home/bergey/code/nixHaskellVersioned/kansas-comet/0.3.1.nix {};
          thisPackage = self.callPackage ./. {};
      };
    };
  in let
     haskellPackages = tmpHaskellPkgs;
     in pkgs.lib.overrideDerivation haskellPackages.thisPackage (attrs: {
       buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
 })
