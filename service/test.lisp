(declaim (optimize (debug 3) (speed 0)))

(setf hunchentoot::*acceptor* (service-acceptor *service*))

(let ((req (make-instance 'hunchentoot:request
                          ;; :uri "/games"
                          :uri "/games/0/moves/0"
                          :headers-in nil
                          :server-protocol "http"
                          :method :get
                          ))
      (reply (make-instance 'hunchentoot:reply)))

  (setf hunchentoot:*request* req
        hunchentoot:*reply* reply
        HUNCHENTOOT::*HUNCHENTOOT-STREAM* t
        ;; hunchentoot::REMOTE-ADDR "localhost"
        )

  (hunchentoot:process-request req)
  (format t "reply: ~A~%" reply)
  )

  (setf hunchentoot::*acceptor* nil)
