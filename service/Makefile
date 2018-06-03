SHELL=/bin/bash


all:
	mkdir -p bin
	buildapp --eval '(declaim (optimize (speed 3)))' --eval '(load #p"~/quicklisp/setup.lisp")' --eval '(load #p"tetris.asd")' --eval "(ql:quickload 'tetris-ai-rest)" --eval '(disable-debugger)' --entry tetris-ai-rest:main --output bin/tetris-ai-rest

clean:
	rm -r bin
