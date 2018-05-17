(in-package :tetris-ai-rest)

(defclass session (hunchensocket:websocket-resource)
  ((game-exc :initarg :game-exc :initform (error "no gameexc") :reader session/game-exc)))

(defun init-session (request)
  (vom:debug "initializing new session... ~A" request)

  (let ((log-fh (service-log-fh *service*) ))
    (when log-fh
      (let ((ip (hunchentoot:remote-addr*))
            (date (hunchentoot:rfc-1123-date))
            (user-agent (hunchentoot:user-agent))
            (path (hunchentoot:script-name*)))
        (format log-fh "~{~A~^	~}" (list ip date user-agent path))
        (force-output log-fh))))

  (ppcre:register-groups-bind ((#'parse-integer game-no))
      ("/games/([0-9]+)" (hunchentoot:script-name*))
    (let* ((game-exc (gethash game-no (service-game-executions *service*))))
      (make-instance 'session :game-exc game-exc))))

(pushnew 'init-session hunchensocket:*websocket-dispatch-table*)

(defun send-hunchensocket-message (conn data)
  ;; https://github.com/cicakhq/potato/blob/master/src/potato/ws-server.lisp
  (check-type conn session)
  (let ((clients (hunchensocket:clients conn)))
    (cond ((null clients)
           (vom:warn "No recipient of message"))
          ((null (cdr clients))
           (hunchensocket:send-text-message (car clients) data))
          (t
           (vom:error "More than one client: ~s" clients)
           ;;TODO close connection
           '(close-hunchensocket-connection conn)))))

(defmethod hunchensocket:client-connected ((conn session) user)
  (vom:debug "ws client connected"))

(defmethod hunchensocket:client-disconnected ((conn session) user)
  (vom:debug "ws client disconnected"))

(defmethod hunchensocket:text-message-received ((conn session) user message)
  (let* ((move-no (parse-integer message))
         (game-exc (session/game-exc conn)))
    (multiple-value-bind (ret-code data) (game-exc-move game-exc move-no)
      (if (not (= 200 ret-code))
          (send-hunchensocket-message conn (format nil "error ~D: ~A" ret-code data))
          (let* ((game-move data)
                 (packed (tetris-ai:game-move-pack game-move)))
            (send-hunchensocket-message conn (write-to-string packed)))))))
