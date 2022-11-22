{ pkgs ? import <nixpkgs> {} }:

with rec {
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "a20de23b925fd8264fd7fad6454652e142fd7f73";
    sha256 = "sha256-8DFJjXG8zqoONA1vXtgeKXy68KdJL5UaXR8NtVMUbx8=";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
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
  src = gitignoreSource ./.;
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
