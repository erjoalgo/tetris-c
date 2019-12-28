#!/bin/bash

set -euo pipefail

if ! command -v autoreconf || ! command -v libtoolize; then
    sudo apt-get install -y autoconf libtool
fi
autoreconf --install && ./configure && make
sudo make install
sudo ldconfig
ldconfig -p | grep libtetris
# check that binary was installed
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
tetris-ai-service -h
echo "built server binary: tetris-ai-service"
