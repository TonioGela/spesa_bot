let
  pins = import ./npins;
  pkgs = import pins.nixpkgs { };
in
pkgs.mkShell {
  inputsFrom = [ (import ./package.nix) ];
  hardeningDisable = [ "fortify" ]; # Scala Native compiles its runtime without -O
  TESSDATA_PREFIX = "${pkgs.tesseract}/share/tessdata";
}
