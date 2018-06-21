#!/bin/bash -x

set -euo pipefail

OUT=${1:-tetris-ai-rest}

buildapp --eval '(declaim (optimize (speed 3)))' \
         --eval '(load #p"~/quicklisp/setup.lisp")' \
         --eval '(load #p"tetris.asd")' \
         --eval "(ql:quickload 'tetris-ai-rest)" \
         --eval "(disable-debugger)" \
         --entry tetris-ai-rest:main  \
         --output ${OUT}
