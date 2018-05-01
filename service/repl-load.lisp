(progn (ql:quickload "hunchentoot") (ql:quickload "cffi")
       (ql:quickload "drakma") (ql:quickload "stefil")
       (ql:quickload "jonathan")
       (ql:quickload "vom")
       (load "libtetris.lisp")
       (load "server.lisp")
       (load "test/server-test.lisp")
       (swank:set-package "SERVER-test")
       )

(progn
  (service-start nil :port 4242)
  (game-create-run-thread 0 :ai-move-delay-secs .5))
