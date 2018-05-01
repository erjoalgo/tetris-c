(progn (ql:quickload "hunchentoot") (ql:quickload "cffi")
       (ql:quickload "drakma") (ql:quickload "stefil")
       (ql:quickload "jonathan")
       (ql:quickload "vom")
       (load "libtetris.lisp")
       (load "server.lisp")
       (load "test/server-test.lisp")
       (swank:set-package "SERVER-test")
       )


(defun s-starts-with-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix (subseq string 0 (length prefix)))))

;; (defun kill-tetris-threads ()
;;   (loop for thread in (sb-thread:list-all-threads)
;;      as name = (sb-thread:thread-name thread)
;;      if (s-starts-with-p server::thread-name-prefix name)
;;      do (progn (format t "killing ~A~%" name)
;;                (sb-thread:terminate-thread thread))))

(progn
  (service-start nil :port 4242)
  (game-create-run-thread 0 :ai-move-delay-secs .5))
