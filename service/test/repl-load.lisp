(declaim (optimize (debug 3) (speed 0)))

(load #p"tetris.asd")
(ql:quickload 'tetris-ai-rest)
(ql:quickload 'tetris-ai-rest-test)

;; (in-package #:tetris-ai-rest)

'(progn
  ;; (setf hunchentoot:*catch-errors-p* nil)
  ;; (vom:config t :debug4)
  (service-stop)
  (service-start nil :port 4242
   :MAX-MOVE-CATCHUP-WAIT-SECS 10
   :grid-height-width (cons 10 10)
   ;; :shapes-file "../one-shape.in"
   )
  (game-create-run-thread 0 :ai-move-delay-secs .5))

'(let ((out "./doc/README.org"))
  (ql:quickload "clod")
  (when (probe-file out) (delete-file out))
  (document-package 'tetris-ai-rest out
   :author "Ernesto Alfonso"
   :email  "erjoalgo@gmail.com"
   ))
