{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.mkShell {
    buildInputs = with nixpkgs; [
      cabal-install
      ghc
      binutils
      delta
      zlib
      zlib.dev
      zlib.out
      haskellPackages.haskell-language-server
    ];
    LD_LIBRARY_PATH = "${nixpkgs.stdenv.cc.cc.lib}/lib;${nixpkgs.zlib}/lib";
  }
