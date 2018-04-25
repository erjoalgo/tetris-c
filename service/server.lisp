(defpackage #:server
  (:use :cl)
  ;; (:use :cl :hunchentoot)
  (:export
   ;; #:file-string #:list-directory-recursive #:list-directory #:list-to-hash-table
   )
  )

(defvar acceptor nil)

(unless acceptor
  (setf acceptor (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))))

(defvar dbg)

(hunchentoot:define-easy-handler (tetris :uri "/tetris.uwsgi") ()
  (setf dbg (list hunchentoot:*request* hunchentoot:*acceptor*))
  ;; (format t "holahola~%" )
  (let* ((qstring (hunchentoot:query-string*)))
    (cond
      ((string= "-1" qstring) "init request")
      (t (or (ppcre::register-groups-bind (move-no game-no) ("([0-9]+)%([0-9]+)" qstring)
               (format nil "game ~A, move ~A~%" game-no move-no))
             "invalid request")))))

(defun test-curl ()
  (with-output-to-string
      (output-fh)
    (SB-EXT:RUN-PROGRAM "curl"
                        (list "localhost:4242/tetris.uwsgi?-1")
                        ;; (list "localhost:4242/tetris.uwsgi?124%123")
                        ;; (list "localhost:4242/yo")
                        :search t
                        :wait t
                        :output output-fh
                        :error output-fh)))

(format t "test curl: ~A~%" (test-curl))
