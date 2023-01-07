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

{
  description = "Hraban's git tools";

  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:hercules-ci/gitignore.nix";
    };
    hly-nixpkgs.url = "github:hraban/nixpkgs/feat/lisp-packages-lite";
  };

  outputs = { self, nixpkgs, flake-utils, gitignore, hly-nixpkgs, ... }:
    with rec {
      ignoredPaths = [ "/build.lisp" "/build-asdf-only.lisp" "/ql-install-deps.lisp" ];
    };
    flake-utils.lib.eachDefaultSystem
      (system:
        with rec {
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (pkgs.callPackage hly-nixpkgs {}) lispPackagesLite;
          ignorePaths = pkgs.callPackage ./ignore-paths.nix {};
          cleanSource = src: ignorePaths {
            src = (pkgs.lib.cleanSource src);
            inherit ignoredPaths;
          };
        };
        with lispPackagesLite;
        {
          packages.default = lispDerivation {
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
            meta = {
              license = "AGPLv3";
            };
          };
        });
}
