let pkgs = import (import ./npins).nixpkgs { }; in
pkgs.mkShell {
  inputsFrom = [ (import ./package.nix { inherit pkgs; }) ];
  hardeningDisable = [ "fortify" ]; # Scala Native compiles its runtime without -O
  TESSDATA_PREFIX = "${pkgs.tesseract}/share/tessdata";
}
