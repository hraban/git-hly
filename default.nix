# I’m a total Nix noob so this is probably in dire need of improvement.

{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let lisp = lispPackages_new.sbclWithPackages (ps : [
      ps.alexandria
      ps.arrow-macros
      ps.inferior-shell
      ps.trivia
      ps.uiop
    ]);
in
stdenv.mkDerivation rec {
  pname = "hly-git-tools";
  version = "0.0.1";
  src = ./.;
  # SBCL likes having a writable home dir to cache .fasl files. Nix build runs
  # with a non-existing homedir. I’m sure we could tell either one not to do
  # that. Or we can just use the current (tmp) dir as HOME, which will
  # automatically be cleaned after build. W/e.
  buildPhase = ''
    export HOME="$PWD"
    ${lisp}/bin/sbcl --noinform --script build-asdf-only.lisp
  '';
  installPhase = ''
    mkdir -p "$out/bin"
    find dist -mindepth 1 -maxdepth 1 -exec cp -Pf {} "$out/bin/" \;
  '';
  dontStrip = true;
}
