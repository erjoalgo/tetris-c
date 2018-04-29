(defpackage #:server
  (:use :cl)
  ;; (:use :cl :hunchentoot)
  (:export
   #:service-start
   #:service-stop
   #:game-create-run
   #:game-create-run-thread
   #:make-config
   ;; #:file-string #:list-directory-recursive #:list-directory #:list-to-hash-table
   )
  )

(in-package #:server)

(defstruct config
  port
  shapes-file
  seed
  grid-dimensions
  max-move-catchup-wait-secs
  )

(defvar config-default
  (make-config :port 4242
               :shapes-file nil ; use libtetris default
               :grid-dimensions nil ; use libtetris default
               :max-move-catchup-wait-secs 10))

(defstruct service
  config
  acceptor
  curr-game-no
  game-executions
  )

(defstruct game-execution
  game
  moves
  thread
  last-recorded-state
  final-state
  running-p
  mutex

  max-moves
  ai-move-delay-secs
  )

;; (defstruct game-execution-opts)

(defstruct last-recorded-state
  timestamp
  on-cells
  move-no
  )

(defun main (argv)
  (declare (ignore argv))
  ;; TODO parse args
  (let ((config config-default))
    (service-start config))
  (loop do (game-create-run)))

(defvar *service* nil)

(defun service-start (config)
  (apply 'libtetris:init-tetris
         (append (when (config-shapes-file config)
                   (list :shapes-file (config-shapes-file config)))
                 (when (config-seed config)
                   (list :seed (config-seed config)))))

  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port (config-port config))))
    (hunchentoot:start acceptor)
    (setf *service*
          (make-service :config (or config config-default)
                        :acceptor acceptor
                        :game-executions (make-hash-table)
                        :curr-game-no 0))))

(defun service-stop (service)
  (let ((acceptor (service-acceptor service)))
    (when (and acceptor (hunchentoot:started-p acceptor))
      (hunchentoot:stop acceptor)))
  (loop for game-exc being the hash-values of (service-game-executions *service*)
     as thread = (game-execution-thread game-exc)
     if thread do
       (sb-thread:terminate-thread thread)))

(defmacro define-regexp-route (name (url-regexp &rest capture-names) &body body)
  `(progn
     (defun ,name ()
       (ppcre:register-groups-bind ,capture-names
           (,url-regexp (hunchentoot:script-name*))
         ,@body))
     (push (hunchentoot:create-regex-dispatcher ,url-regexp ',name)
           hunchentoot:*dispatch-table*)))

(defun json-resp (return-code body)
  (when return-code
    (setf (hunchentoot:return-code*) return-code))
  (setf (hunchentoot:content-type*) "application/json")
  (jonathan:to-json body))

(defun game-execution-last-recorded-state-blocking (game-exc &key (timeout-secs 10))
  (if (not (game-execution-running-p game-exc))
      (game-execution-final-state game-exc)
      (let ((mutex (game-execution-mutex game-exc))
            last-state)
        (sb-thread:with-mutex (mutex)
          (setf (game-execution-last-recorded-state game-exc) nil))
        (loop
           with checks-per-sec = 2
           with check-delay = (/ 1 checks-per-sec)
           with max-i = (max (* checks-per-sec timeout-secs) 1)

           until (setf last-state (game-execution-last-recorded-state game-exc))
           for i below max-i
           do (format t "waiting for last state~%")
           do (sleep check-delay))
        (or last-state (game-execution-final-state game-exc)))))

(define-regexp-route current-game-state-handler ("^/games/([0-9]+)$"
                                                 (#'parse-integer game-no))
  (let* ((game-exc (gethash game-no (service-game-executions *service*))))
    (if (null game-exc)
        (json-resp hunchentoot:+HTTP-NOT-FOUND+
                   '(:error "no such game"))
        (let ((game (game-execution-game game-exc))
              (last-recorded-state (game-execution-last-recorded-state-blocking game-exc))
              (move-no 0)
              (on-cells nil))
          (when last-recorded-state
            (setf move-no (last-recorded-state-move-no last-recorded-state)
                  on-cells (last-recorded-state-on-cells last-recorded-state)))
          (json-resp nil (append on-cells
                                 (list (libtetris:game-height game)
                                       (libtetris:game-width game)
                                       move-no game-no)))))))

(define-regexp-route game-move-handler ("^/games/([0-9]+)/moves/([0-9]+)$"
                                        (#'parse-integer game-no) (#'parse-integer move-no))
  ;; (setf (hunchentoot:content-type*) "text/plain")
  (let* ((game-exc (gethash game-no (service-game-executions *service*)))
        (moves (and game-exc (game-execution-moves game-exc))))
    (if (null game-exc)
        (json-resp hunchentoot:+HTTP-NOT-FOUND+ '(:error "no such game"))
          (cond
            ((and (not (game-execution-running-p game-exc)) (>= move-no (length moves)))
             (json-resp hunchentoot:+HTTP-REQUESTED-RANGE-NOT-SATISFIABLE+
                        '(:error "requested move outside of range of completed game")))
            (t (loop with
                  max-move-catchup-wait-secs = (config-max-move-catchup-wait-secs
                                                (service-config *service*))
                  for i below max-move-catchup-wait-secs
                  as behind = (>= move-no (length moves))
                  while behind
                  do (progn
                       (format t "catching up from ~D to ~D on game ~D (~D secs left)~%"
                               (length moves) move-no game-no (- max-move-catchup-wait-secs i))
                            (sleep 1))
                  finally
                    (if behind
                        (json-resp hunchentoot:+HTTP-SERVICE-UNAVAILABLE+
                                   '(:error "reached timeout catching up to requested move~%" ))
                        (return
                          (json-resp nil
                                     (with-slots (libtetris::shape-code libtetris::rot libtetris::col)
                                         (aref moves move-no)
                                       (list libtetris::shape-code libtetris::rot
                                             libtetris::col)))))))))))

(define-regexp-route game-list-handler ("^/games/?$")
  (json-resp nil
             (loop for game-no being the hash-keys of (service-game-executions *service*)
                collect game-no)))

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/index.html" "html/tetris.html")
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/js/tetris_client.js" "js/tetris_client.js")
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/loading.gif" "loading.gif")
      hunchentoot:*dispatch-table*)

(hunchentoot:define-easy-handler (shapes :uri "/shapes") ()
  (libtetris:serialize-shapes))

(defun game-serialize-state (game move-no)
  (make-last-recorded-state
   :timestamp (multiple-value-bind (secs usecs) (sb-ext:get-time-of-day)
                (declare (ignore usecs)) secs)
   :move-no move-no
   :on-cells (libtetris:game-on-cells-packed game)))

(defun game-run (game-exc)
  (setf (game-execution-running-p game-exc) t)
  (with-slots (game moves max-moves mutex ai-move-delay-secs) game-exc
  (loop
     with last-recorded-state-check-multiple = 5
     for i from 0
     as next-move = (libtetris:game-apply-next-move game)
     while (and next-move (or (null max-moves) (< i max-moves)))
     as string = (libtetris:game-printable-string game string)
     do (format t string)
     do
       (let ((native (libtetris:my-translate-from-foreign next-move)))
         (progn
           (format t "on move ~D, shape ~D, rot ~D, col ~D~%" i
                   (slot-value native 'libtetris::shape-code)
                   (slot-value native 'libtetris::rot)
                   (slot-value native 'libtetris::col))
           (unless (zerop ai-move-delay-secs)
             (sleep ai-move-delay-secs))
           (vector-push-extend native moves)))
     if (and (zerop (mod i last-recorded-state-check-multiple))
             (null (game-execution-last-recorded-state game-exc)))
     do
       (sb-thread:with-mutex (mutex)
         (setf (game-execution-last-recorded-state game-exc)
               (game-serialize-state game i)))
     finally
       (setf (game-execution-running-p game-exc) nil
             (game-execution-final-state game-exc) (game-serialize-state game i)))))

(defun game-create (game-no &key max-moves (ai-move-delay-secs .5))
  (let ((moves (make-array 0 :adjustable t
                           :fill-pointer t
                           :element-type 'libtetris:game-move-native))
        (game (libtetris:game-init libtetris:HEIGHT libtetris:WIDTH libtetris:ai-default-weights)))
    (setf (gethash game-no (service-game-executions *service*))
          (make-game-execution :game game
                               :moves moves
                               :max-moves max-moves
                               :ai-move-delay-secs ai-move-delay-secs
                               :mutex (sb-thread:make-mutex)))))

(defun game-create-run (&optional game-no &rest create-args)
  (let ((game-no (or game-no (incf (service-curr-game-no *service*)))))
    (when (gethash game-no (service-game-executions *service*))
      (error "game ~D exists" game-no))
    (game-run (apply 'game-create game-no create-args))))

(defun game-create-run-thread (&optional game-no &rest create-args)
  (let* ((game-exc (apply 'game-create game-no create-args)))
    (values
     (setf (game-execution-thread game-exc)
          (sb-thread:make-thread 'game-run :arguments (list game-exc)
                                 :name (format nil "game ~D" game-no)))
     game-exc)))

