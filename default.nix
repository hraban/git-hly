{ pkgs ? import <nixpkgs> {} }:

with rec {
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "a20de23b925fd8264fd7fad6454652e142fd7f73";
    sha256 = "sha256-8DFJjXG8zqoONA1vXtgeKXy68KdJL5UaXR8NtVMUbx8=";
  };
  cleanSource = src: (pkgs.callPackage gitignoreSrc {}).gitignoreSource (pkgs.lib.cleanSource src);

  hPkgsSrc = pkgs.fetchFromGitHub {
    owner = "hraban";
    repo = "nixpkgs";
    rev = "a338dfe8d295f60c59cc43233309ceadb3c3ea2c";
    sha256 = "cJFG9g6s0i5Om8AWAez7//4URdlezCp6awfS50kkH5o=";
  };
  inherit (pkgs.callPackage hPkgsSrc {}) lispPackagesLite;
};

with lispPackagesLite;

lispDerivation {
  lispSystem = "git-hly";
  pname = "git-hly";
  version = "0.0.1";
  src = cleanSource ./.;
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
