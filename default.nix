{ pkgs ? import <nixpkgs> {} }:

with rec {
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "a20de23b925fd8264fd7fad6454652e142fd7f73";
    sha256 = "sha256-8DFJjXG8zqoONA1vXtgeKXy68KdJL5UaXR8NtVMUbx8=";
  };
  cleanSource = src: (pkgs.callPackage gitignoreSrc {}).gitignoreSource (pkgs.lib.cleanSource src);

  lispPackagesLite = import (pkgs.fetchFromGitHub {
    owner = "hraban";
    repo = "cl-nix-lite";
    rev = "40b368ed386ff3e7739985f6633e0625f8534f2c";
    sha256 = "sha256-11/0ZTvxrI5PEhzOKUbZj0IAdiMiLpGGSs576LwFZ6w=";
  }) { inherit pkgs; };
};

with lispPackagesLite;

lispDerivation {
  lispSystem = "git-hly";
  pname = "git-hly";
  version = "0.0.1";
  src = cleanSource ./.;
  dontPatchShebangs = true;
  dontStrip = true;
  lispDependencies = [
    alexandria
    arrow-macros
    inferior-shell
    trivia
    trivia-ppcre
  ];
}
