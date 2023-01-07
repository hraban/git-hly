# Copyright © 2023  Hraban Luyat
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

# Wrapper for a lib.sources.cleanSourceWith filter which strips the
# /nix/store/abcabc-blabla-1.2.3 parth from a path. End result:
# /foo/bar/baz (iow starts with a slash).
{ lib }:

with rec {
  # This would have been easier with a passive group, aka (?:...), but
  # Nix’s regex engine doesn’t support that. 🤷
  stripStorePath = f: path: f (lib.lists.last (builtins.match "^(/[^/]*){3}(.*)$" path));
};
{ src, ignoredPaths, ...} @ args:
lib.sources.cleanSourceWith ((removeAttrs args ["ignoredPaths"]) // {
  filter = stripStorePath (path: type:
    (! builtins.elem path ignoredPaths));
})

