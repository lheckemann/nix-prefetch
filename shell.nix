let
  pkgs = import <nixpkgs> {};
  ghc = pkgs.ghc.withPackages (ps: with ps; [
    # Library deps
    hnix typed-process base16-bytestring megaparsec
    parser-combinators
  ]);
in pkgs.runCommand "nix-prefetch" {
  buildInputs = [ghc];
  NIX_GHC = "${ghc}/bin/ghc";
  NIX_GHCPKG = "${ghc}/bin/ghc-pkg";
  NIX_GHC_LIBDIR = "${ghc}/lib/ghc-${ghc.version}";
} ":"
