(in-package :tetris-ai-rest)

(defclass exc-resource (clws:ws-resource)
  ((game-exc :initarg :game-exc :reader exc-resource/game-exc)))

(defstruct ws-service
  listener-thread
  resource-threads)

(defun ws-start (port)
  ;; TODO sb threads
  (make-ws-service
   :listener-thread
   (bordeaux-threads:make-thread
    (lambda ()
      (clws:run-server port))
    :name "tetris websockets server")))

(defun ws-stop (service)
  (bordeaux-threads:destroy-thread (ws-service-listener-thread service))
  (loop for thread in (ws-service-resource-threads service) do
       (bordeaux-threads:destroy-thread thread)))

(defun ws-register-game (game-no ws-service &aux game-exc)
  (vom:debug "initializing new session... ~D" game-no)

  (setf game-exc (gethash game-no (service-game-executions *service*)))
  (let ((path (format nil "/games/~D" game-no)))
    (clws:register-global-resource
     path
     (make-instance 'exc-resource :game-exc game-exc)

     ;; TODO do I need this?
     (clws:origin-prefix "http://127.0.0.1" "http://localhost"))

    (push (bordeaux-threads:make-thread
           (lambda ()
             (clws:run-resource-listener
              (clws:find-global-resource path)))
           :name (format nil "resource listener for ~A" path))
          (ws-service-resource-threads ws-service))))


(defmethod clws:resource-client-connected ((res exc-resource) client)
  (vom:debug "ws client connected")

  (let ((log-fh (service-log-fh *service*) ))
    (when log-fh
      (let ((ip (clws:client-host client))
            (date (hunchentoot:rfc-1123-date))
            (user-agent (gethash :user-agent (clws:client-connection-headers client)))
            (path (clws:client-resource-name client)))
        (format log-fh "~{~A~^	~}~%" (list ip date user-agent path))
        (force-output log-fh)))))

(defmethod clws:resource-client-disconnected ((resource exc-resource) (client clws::client))
  (vom:debug "ws client disconnected"))

(defmethod clws:resource-received-text ((res exc-resource) client message)
  ;; (vom:debug "new received text ~A" message)
  (let* ((move-no (parse-integer message))
         (game-exc (exc-resource/game-exc res)))
    (multiple-value-bind (ret-code data) (game-exc-move game-exc move-no)
      (if (not (= 200 ret-code))
          (clws:write-to-client-binary client (- ret-code))
          (let* ((game-move data)
                 (packed (tetris-ai:game-move-pack game-move)))
            (clws:write-to-client-text client (write-to-string packed)))))))

(defmethod clws:resource-received-binary ((res exc-resource) client message)
  (vom:debug "received binary: ~A" message)
  )
