SHELL=/bin/bash

all: bin/tetris-ai-service

bin/tetris-ai-service:
	mkdir -p bin
	buildapp --eval '(declaim (optimize (speed 3)))' --eval '(load #p"~/quicklisp/setup.lisp")' --eval '(load #p"tetris.asd")' --eval "(ql:quickload 'tetris-ai-rest)" --eval "(disable-debugger)" --entry tetris-ai-rest:main --output bin/tetris-ai-service

clean:
	rm -r bin

install: bin/tetris-ai-service
	cp -f $< /usr/local/bin/
