{ pkgs ? import <nixpkgs> {} }:

with rec {
  hpkgs = import (
    pkgs.fetchFromGitHub {
      owner = "hraban";
      repo = "nixpkgs";
      rev = "1b768e079888cdea9e1ebed635168e1139c98130";
      sha256 = "sha256-VhVKh0Vd+OtaGNhxv+6ZEyUCC7AalNJ7AQOcBlfbZrY=";
    }
  ) {};
  lispPackagesLite = hpkgs.lispPackagesLite.override {
    inherit pkgs;
  };
};

with lispPackagesLite;

lispDerivation {
  lispSystem = "git-hly";
  pname = "git-hly";
  version = "0.0.1";
  src = pkgs.lib.cleanSource ./.;
  lispDependencies = [
    alexandria
    arrow-macros
    inferior-shell
    trivia
    trivia-ppcre
  ];
  installPhase = ''
    mkdir -p "$out/bin"
    cp dist/* "$out/bin/"
  '';
}
