{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  dfltSrc ? ./.,
  gcc ? pkgs.gcc,
  gmp ? pkgs.gmp,
  gnumake ? pkgs.gnumake,
  binutils ? pkgs.binutils-unwrapped,
  bash ? pkgs.bash,
  mlton ? pkgs.mlton
}:

stdenv.mkDerivation rec {
  name = "mlton";

  src = dfltSrc;

  buildInputs = [ gcc gmp gnumake binutils bash mlton ];

  buildPhase = ''
    find . -type f | grep -v -e '\.tgz''$' | xargs sed -i "s@/usr/bin/env bash@$(type -p bash)@"
    make dirs
    make runtime -j
    make
  '';

  installPhase = ''
    make install PREFIX=$out
  '';

}
