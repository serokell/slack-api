with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "slack-api";

  buildInputs = [
    haskell.compiler.ghc843
    cabal-install
    zlib
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${zlib}/lib:$LD_LIBRARY_PATH
  '';
}
