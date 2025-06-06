{ pkgs, lib, agda-tools }:

{ name, description, src, output-pdf-name ? "*.pdf" }:

pkgs.stdenv.mkDerivation {

  inherit name;
  inherit description;

  src = lib.sourceFilesBySuffices src [
    ".tex"
    ".bib"
    ".cls"
    ".bst"
    ".pdf"
    ".png"
    ".agda"
    ".agda-lib"
    ".lagda"
    ".latexmkrc"
    "Makefile"
  ];

  buildInputs = [
    pkgs.texliveFull
    pkgs.zip
    agda-tools.agda-with-stdlib # Some papers need to compile Agda
  ];

  installPhase = ''
    mkdir -p $out
    make clean
    make
    cp ${output-pdf-name} $out/
  '';

  meta = with lib; {
    inherit description;
    license = licenses.asl20;
  };
}
