{ pkgs ? import <nixpkgs> {} }: with pkgs;

let
  hsPkgs = ps: with ps; [
    containers
    aeson
    shake
    turtle
  ];
in
stdenv.mkDerivation {
  name = "applying-fp-patterns";
  buildInputs = [
    fasd
    (texlive.combine {
        inherit (texlive)
        scheme-medium
        beamer
        listings
        minted
        cleveref
        microtype
        babel
        todonotes
        chngcntr
        excludeonly
        upquote
        ifplatform
        xstring
        enumitem;
      })
      zathura
  ] ++ (with haskellPackages; [
    (ghcWithPackages hsPkgs)
  ]);
}
