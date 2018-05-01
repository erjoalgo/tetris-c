(load #p"tetris.asd")
(ql:quickload 'tetris-ai-rest)

'(progn
  ;; (setf hunchentoot:*catch-errors-p* nil)
  ;; (vom:config t :debug4)
  (service-stop)
  (service-start nil :port 4242 :MAX-MOVE-CATCHUP-WAIT-SECS 1)
  (game-create-run-thread 0 :ai-move-delay-secs .5))
