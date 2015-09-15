let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  ghc = pkgs.haskellPackages.ghcWithPackages
    (haskellPackages: with haskellPackages; [ hspec ]);

in rec {
  ifltEnv = stdenv.mkDerivation rec {
    name = "iflt-env";
    version = "1.1.1.1";
    src = ./.;
    buildInputs = [
      ghc
    ];
  };
}
