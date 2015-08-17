let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  ghc = pkgs.haskellPackages.ghcWithPackages
    (haskellPackages: with haskellPackages; [ hspec ]);

in rec {
  # miranda = stdenv.mkDerivation rec {
  #   name = "miranda-${version}";
  #   version = "2.042";
  #
  #   src = pkgs.fetchurl {
  #     url    = "http://www.cs.kent.ac.uk/people/staff/dat/miranda/downloads/linux/mira-2042-i686-Linux.tgz";
  #     sha256 = "1eafb0205e42fd8d72cb0ccf8a13ddc54f3bb48148df09c2f3ec365b549db27e";
  #   };
  #
  #   installPhase = ''
  #     mkdir -p $out/bin
  #     cp bin/just bin/mtotex $out/bin/
  #     cp bin/mira $out/bin/mira-unwrapped
  #
  #     makeWrapper $out/bin/mira-unwrapped $out/bin/mira \
  #       --add-flags "-lib $out/lib/miralib"
  #
  #     cp -R share $out/share
  #     cp -R lib $out/lib
  #   '';
  #
  #   postFixup = ''
  #     $out/bin/mira -make
  #   '';
  #
  #   buildInputs = [ pkgs.makeWrapper ];
  #
  #   meta = {
  #     description = "";
  #     homepage = "http://miranda.org.uk";
  #     license = stdenv.lib.licenses.unfree;
  #   };
  # };

  ifltEnv = stdenv.mkDerivation rec {
    name = "iflt-env";
    version = "1.1.1.1";
    src = ./.;
    buildInputs = [
      ghc
    ];
  };
}
