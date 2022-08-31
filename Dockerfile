FROM clfoundation/sbcl:2.1.5 AS build

ENV DEBIAN_FRONTEND=noninteractive

#----------------------- COMMON LISP & QUICKLISP ONLY

# Install ONLY SBCL and quicklisp. I'm trying to keep the rest of the app
# dependencies out of this part, for maintainability. The executable
# /ql/ql-install-deps.lisp will download all dependencies of all local quicklisp
# projects. Usage: copy /root/quicklisp/local-projects/your-app/your-app.asd, then
# run that script, then copy the rest of the source files (for better Docker
# layer caching).

WORKDIR /root


COPY ql-install-deps.lisp quicklisp-release-key.txt ./

# Install Quicklisp
RUN apt-get update
RUN apt-get install -y --no-install-recommends curl gpg
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
RUN gpg --import quicklisp-release-key.txt
RUN gpg --verify quicklisp.lisp.asc quicklisp.lisp
RUN sbcl --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)'

# And Docker, for the docker-cli, which is super useful. Install the whole
# jigmajog in the build image, and just copy out the final binary 🤷‍♀️ it’s go so
# it’s statically linked, anyway. Easiest method by far.
RUN apt-get install -y --no-install-recommends \
        ca-certificates \
        lsb-release
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg && \
    echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/debian \
  $(lsb_release -cs) stable" > /etc/apt/sources.list.d/docker.list
RUN apt-get update
RUN apt-get install -y --no-install-recommends docker-ce-cli

# Install QL dependencies in a separate layer (for caching)
COPY src/hly-git-tools.asd ./quicklisp/local-projects/hly-git-tools/hly-git-tools.asd
RUN ./ql-install-deps.lisp

# Now compile the app
WORKDIR /app
COPY . .
RUN ./build.lisp

# This is the base image for clfoundation/sbcl. Using the same base image
# simplifies the whole dynamic libs thing.
FROM debian:buster

LABEL org.opencontainers.image.source https://github.com/zwaluy/btcarb

ENV DEBIAN_FRONTEND=noninteractive

# Install OS managed dependencies
RUN useradd -d /ql -m ql

USER ql

WORKDIR /app

COPY --from=build /app/build/hly-git-tools /app/hly-git-tools

ENTRYPOINT ["/app/hly-git-tools"]

CMD []

# Copyright © 2022  Hraban Luyat
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
