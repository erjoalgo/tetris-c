#!/bin/bash -x

set -euo pipefail

libtoolize
# aclocal
# autoconf
# automake --add-missing
autoreconf --install || exit 1
