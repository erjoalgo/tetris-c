SHELL=/bin/bash
# LIB_OPTS=$(shell for lib in libs/*; do echo -n --asdf-path $$lib/" "; done)

all:
	mkdir -p bin
	buildapp --eval '(declaim (optimize (speed 3)))' --eval "(require 'asdf)" --asdf-path "." --eval '(load #p"~/quicklisp/setup.lisp")' --eval "(ql:quickload 'tetris)" --entry server:main --output bin/tetris-server
# --dynamic-space-size 3240
# --control-stack-size 100000

clean:
	rm -r bin
