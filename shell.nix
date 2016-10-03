{ pkgs ? import <nixpkgs> {} }: with pkgs;

let
  hsPkgs = ps: with ps; [
    containers
    aeson
    shake
    turtle
  ];
  tex = texlive.combine {
    inherit (texlive)
    babel
    beamer
    # calc
    chngcntr
    cleveref
    enumitem
    etoolbox
    excludeonly
    fancyvrb
    float
    framed
    # fvextra
    ifplatform
    # ifthen
    # keyval
    # kvoptions
    lineno
    listings
    microtype
    minted
    # pdftexcmds
    scheme-medium
    todonotes
    upquote
    xcolor
    xstring;
  };
in
stdenv.mkDerivation {
  name = "applying-fp-patterns";
  buildInputs = [
    tex
    which
  ] ++ (with haskellPackages; [
    (ghcWithPackages hsPkgs)
  ]) ++ (with pythonPackages; [
    pygments
  ]);
}
