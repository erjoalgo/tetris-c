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
   :grid-dimensions (cons 10 10)
   ;; :shapes-file "../one-shape.in"
   )
  (game-create-run-thread 0 :ai-move-delay-secs .5))
