#!/bin/bash -x

set -euo pipefail

pip install --user gprof2dot
sudo apt-get install -y graphviz linux-tools-3.16

if ! command -v pprof; then
    mkdir -p ~/src && cd ~/src
    test -d gperftools || git clone https://github.com/gperftools/gperftools
    cd gperftools
    ./autogen.sh
    sudo apt-get install -y libunwind-dev
    ./configure
    make
    sudo make install
fi

