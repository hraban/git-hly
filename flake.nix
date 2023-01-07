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
    cl-nix-lite = {
      flake = false;
      url = "github:hraban/cl-nix-lite";
    };
  };

  outputs = { self, nixpkgs, flake-utils, gitignore, cl-nix-lite, ... }:
    with rec {
      ignoredPaths = [
        ./build.lisp
        ./build-asdf-only.lisp
        ./ql-install-deps.lisp
      ];
    };
    flake-utils.lib.eachDefaultSystem (system:
      with rec {
        pkgs = nixpkgs.legacyPackages.${system};
        lispPackagesLite = import cl-nix-lite { inherit pkgs; };
        cleanSource = src: pkgs.lib.cleanSourceWith {
          filter = path: type: (! builtins.elem (/. + path) ignoredPaths);
          src = pkgs.lib.cleanSource (gitignore.lib.gitignoreSource src);
        };
      };
      {
        packages = {
          # This is for normal people
          default = with lispPackagesLite; lispDerivation {
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
            dontStrip = true;
            meta = {
              license = pkgs.lib.licenses.agpl3Only;
            };
          };
        };
      });
}
