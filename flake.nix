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
    gitignore = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:hercules-ci/gitignore.nix";
    };
    cl-nix-lite.url = "github:hraban/cl-nix-lite";
    flake-utils = {
      url = "flake-utils";
      inputs.systems.follows = "systems";
    };
    systems.url = "systems";
  };

  outputs = { self, nixpkgs, flake-utils, gitignore, cl-nix-lite, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      with rec {
        pkgs = nixpkgs.legacyPackages.${system}.extend cl-nix-lite.overlays.default;
        inherit (pkgs) lispPackagesLite;
        cleanSource = src: pkgs.lib.pipe src [ gitignore.lib.gitignoreSource pkgs.lib.cleanSource ];
      };
      {
        packages.default = with lispPackagesLite; lispDerivation {
          lispSystem = "git-hly";
          pname = "git-hly";
          version = "0.0.1";
          src = cleanSource ./.;
          lispDependencies = [
            alexandria
            arrow-macros
            asdf
            inferior-shell
            trivia
            lispPackagesLite."trivia.ppcre"
            wild-package-inferred-system
          ];
          # Override this to to disable the per-child command symlinking
          symlinkCommands = true;
          # I’m not sure if this is genius or awful? If I have to ask, it’s
          # probably awful.
          postBuild = ''
# Ideally, I should be able to access overridden args in the derivation itself
# by passing a callback to lispDerivation, just like stdenv.mkDerivation...
if [[ $symlinkCommands == "1" ]]; then
${pkgs.sbcl}/bin/sbcl --script <<EOF | while read cmd ; do (cd bin && ln -s git-hly git-$cmd) ; done
(require :asdf)
(asdf:load-system "git-hly")
(format T "~{~(~A~)~^~%~}~%" (git-hly/src/cmd::cmd-names))
EOF
fi
'';
          installPhase = "mkdir -p $out; cp -r bin $out/";
          dontStrip = true;
          meta = {
            license = pkgs.lib.licenses.agpl3Only;
          };
        };
      });
}
