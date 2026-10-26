{
  sources ? import ./npins,
  pkgs ? import sources.nixpkgs { },
}:
let
  inherit (pkgs) lib;
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [ ./project.scala ./src ./resources ];
  };
  env = ''
    export HOME=$TMPDIR COURSIER_CACHE=$TMPDIR/coursier
  '';
  package = "scala-cli --power package . --native --server=false -o spesa";
  nativeBuildInputs = [ pkgs.scala-cli pkgs.clang pkgs.which ];
  buildInputs = [ pkgs.tesseract pkgs.leptonica pkgs.s2n-tls ];
  # Fixed-output derivation: fetches compiler + native toolchain into a coursier cache.
  deps = pkgs.stdenv.mkDerivation {
    name = "spesa-deps";
    inherit src nativeBuildInputs buildInputs;
    buildPhase = ''
      ${env}
      ${package}
    '';
    # drop non-deterministic coursier bookkeeping files
    installPhase = ''
      find $COURSIER_CACHE \( -name '*.lock' -o -name '.*.checked' -o -name '*.lastCheck' \) -delete
      cp -r $COURSIER_CACHE $out
    '';
    outputHashMode = "recursive";
    outputHash = lib.fakeHash;
  };
in
pkgs.stdenv.mkDerivation {
  pname = "spesa";
  version = "0.1.0";
  inherit src buildInputs;
  nativeBuildInputs = nativeBuildInputs ++ [ pkgs.makeWrapper ];
  buildPhase = ''
    ${env}
    cp -r ${deps} $COURSIER_CACHE && chmod -R u+w $COURSIER_CACHE
    ${package} --offline
  '';
  installPhase = ''
    install -Dm755 spesa $out/bin/spesa
    wrapProgram $out/bin/spesa --set-default TESSDATA_PREFIX ${pkgs.tesseract}/share/tessdata
  '';
  passthru = { inherit deps; };
  meta.mainProgram = "spesa";
}
