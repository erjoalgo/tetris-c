(ql:quickload "hunchentoot")

(defvar server nil)

(unless server
  (setf server (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))))

(defvar dbg)

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(hunchentoot:define-easy-handler (tetris :uri "/tetris.uwsgi") ()
  (setf (hunchentoot:content-type*) "text/plain")
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
