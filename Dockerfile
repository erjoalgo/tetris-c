FROM debian:latest

RUN apt-get update

# install sbcl & quicklisp
RUN apt-get install -y sbcl cl-quicklisp
RUN sbcl --load /usr/share/common-lisp/source/quicklisp/quicklisp.lisp --eval "(quicklisp-quickstart:install)"
RUN echo "(let ((quicklisp-init (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))) (when (probe-file quicklisp-init) (load quicklisp-init)))" > ~/.sbclrc
RUN sbcl --eval "(describe 'ql:quickload)" --eval "(exit)"

RUN apt-get update && apt-get install -y autoconf libtool make libfixposix-dev

WORKDIR /tetris
COPY . .

# simply load the lisp libraries and dependencies in an early layer for caching
RUN cd service; sbcl --eval  --load "tetris.asd" --eval "(ql:quickload 'tetris-ai-rest)"

# work around https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=1039612;msg=5
RUN unset BASH_VERSION; autoreconf --install && ./configure
RUN make && make install && ldconfig && ldconfig -p | grep tetris

EXPOSE 4242 4243

RUN cp shapes.in /usr/local/share/tetris/shapes.in

CMD cd service; sbcl --eval '(declaim (optimize (speed 3)))'  --load "tetris.asd" --eval "(ql:quickload 'tetris-ai-rest)" --non-interactive --eval "(tetris-ai-rest:main)"; tail -f /dev/null

