#!/bin/bash

set -euo pipefail

GIT_DIR=${HOME}/git/tetris-c

test -d "${GIT_DIR}" ||  \
    git clone https://github.com/erjoalgo/tetris-c "${GIT_DIR}"

cd "${GIT_DIR}"
if ! tetris -h; then
    autoreconf --install && ./configure && make
    sudo make install
    sudo ldconfig
    ldconfig -p | grep libtetris
fi
# check binary was installed
tetris -h

# build, start the service
sudo apt-get install -y buildapp libfixposix-dev
if ! which sbcl && ! ~/.stumpwmrc.d/scripts/installs/sbcl.sh; then
    echo "sbcl, quickload not found"
    exit ${LINENO}
fi
# ensure we have sbcl, quickload
sbcl --eval "(describe 'ql:quickload)" --eval "(exit)"

cd service
make
BINARY=$(pwd)/bin/tetris-ai-rest
test -e ${BINARY}

LOGFILE="tetris" daemonize "${BINARY}"
