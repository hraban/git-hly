# Copyright © 2022, 2023  Hraban Luyat
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, version 3 of the License.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

  winfer = lispPackagesLite.lispDerivation {
    lispSystem = "wild-package-inferred-system";
    src = pkgs.fetchFromGitHub {
      owner = "privet-kitty";
      repo = "wild-package-inferred-system";
      rev = "800b5f89b61cc4a0a9cf14706fd3f7e7fcd3e539";
      sha256 = "AH23HE+NpJ8aZkvyAHWKPtNoQSJatkFXwh6rgeKQ42o=";
    };
  };
};

with lispPackagesLite;

lispDerivation {
  lispSystem = "git-hly";
  pname = "git-hly";
  version = "0.0.1";
  src = cleanSource ./.;
  dontPatchShebangs = true;
  lispDependencies = [
    asdf
    alexandria
    arrow-macros
    inferior-shell
    trivia
    trivia-ppcre
    winfer
  ];
}
