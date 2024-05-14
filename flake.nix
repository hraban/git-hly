# Copyright © 2022–2024  Hraban Luyat
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
    cl-nix-lite.url = "github:hraban/cl-nix-lite";
    flake-utils = {
      url = "flake-utils";
      inputs.systems.follows = "systems";
    };
    systems.url = "systems";
  };

  outputs = { self, nixpkgs, flake-utils, cl-nix-lite, ... }: let
    git-hly = { pkgs, lib }: let
      inherit (pkgs) ecl;
      pkgs' = pkgs.appendOverlays [
        cl-nix-lite.overlays.default
        (pfinal: pprev: {
          lispPackagesLite = pprev.lispPackagesLiteFor ecl;
        })
      ];
      inherit (pkgs') lispPackagesLite;
    in with lispPackagesLite; lispDerivation {
      lispSystem = "git-hly";
      pname = "git-hly";
      version = "0.0.1";
      src = lib.cleanSource ./.;
      lispDependencies = [
        alexandria
        arrow-macros
        asdf
        inferior-shell
        trivia
        lispPackagesLite."trivia.ppcre"
      ];
      lispBuildPhase = ''
        ;; ECL is not a fan of ASDFv3's auto update magic
        (load "${asdf}/build/asdf.lisp")
        (let ((sys "git-hly"))
          (asdf:load-system sys)
          (asdf:make-build sys
                           :type :program
                           :move-here #P"./bin/"
                           :epilogue-code `(progn
                                            (,(read-from-string
                                               (asdf::component-entry-point
                                                (asdf:find-system sys))))
                                            (quit))))
      '';
      # Override this to to disable the per-child command symlinking
      symlinkCommands = true;
      # I’m not sure if this is genius or awful? If I have to ask, it’s
      # probably awful.
      postBuild = let
        namePrinter = pkgs.writeText "print-names.lisp" ''
          (setf *compile-verbose* NIL)
          (setf *load-verbose* NIL)
          (load "${asdf}/build/asdf.lisp")
          (asdf:load-system "git-hly")
          (format T "~{~(~A~)~%~}" (git-hly/src/cmd::cmd-names))
        '';
      in ''
# Ideally, I should be able to access overridden args in the derivation itself
# by passing a callback to lispDerivation, just like stdenv.mkDerivation...
if [[ $symlinkCommands == "1" ]]; then
  ${lib.getExe ecl} --shell ${namePrinter} | while read cmd ; do
    ln -s git-hly bin/git-$cmd
  done
fi
'';
      installPhase = "mkdir -p $out; cp -r bin $out/";
      dontStrip = true;
      meta = {
        license = lib.licenses.agpl3Only;
      };
    };
  in flake-utils.lib.eachDefaultSystem (system:
    with rec {
      pkgs = nixpkgs.legacyPackages.${system};
    };
    {
      packages = {
        default = pkgs.callPackage git-hly { };
      };
    });
}
