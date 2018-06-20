(declaim (optimize (debug 3) (speed 0)))

(progn
  ;; (setf hunchentoot:*catch-errors-p* nil)
  ;; (vom:config t :debug4)
  (load "tetris.asd")
  (ql:quickload "tetris-ai-rest")
  (in-package "TETRIS-AI-REST")
  (funcall (find-symbol
            "SERVICE-START" "TETRIS-AI-REST")
           :port 4242)
  (funcall (find-symbol
            "GAME-CREATE-RUN-THREAD" "TETRIS-AI-REST")))


'(let ((out "./doc/README.org"))
  (ql:quickload "clod")
  (when (probe-file out) (delete-file out))
  (document-package 'tetris-ai-rest out
   :author "Ernesto Alfonso"
   :email  "erjoalgo@gmail.com"
   ))
