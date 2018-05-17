;; server.lisp ---

;; Copyright (C) 2018 Ernesto Alfonso <erjoalgo@gmail.com>

;; Author: Ernesto Alfonso <erjoalgo@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

(defpackage #:tetris-ai-rest
  (:use :cl)
  (:export
   #:service-start
   #:service-stop
   #:game-create-run
   #:game-create-run-thread
   #:make-config
   #:service-config
   #:grid-height-width
   #:config-grid-height-width
   #:game-serialize-state
   )
  )

(in-package #:tetris-ai-rest)

(defstruct config
  port
  ws-port
  shapes-file
  seed
  grid-height-width
  max-move-catchup-wait-secs
  ai-depth
  default-ai-move-delay-millis
  )

(defvar config-default
  (make-config :port 4242
               ;; :ws-port 4243
               :shapes-file tetris-ai:default-shapes-file
               :grid-height-width (cons tetris-ai:default-height
                                      tetris-ai:default-width)
               :ai-depth 3
               :default-ai-move-delay-millis 500
               :max-move-catchup-wait-secs 10)
  "fallback service configuration to fill in any mising (nil) values"
  )

(defstruct service
  config
  acceptor
  ws-acceptor
  curr-game-no
  game-executions
  )

(defstruct game-execution
  game
  moves
  last-recorded-state
  final-state
  running-p

  max-moves
  ai-move-delay-secs
  last-recorded-state-check-delay-secs
  )

(defstruct last-recorded-state
  timestamp
  on-cells
  ;; on-cells-cnt
  move-no
  )

(defvar thread-name-prefix "tetris-game-thread" "prefix for name of tetris worker threads")

(defvar *service* nil "the default currently active service")

(defun service-start (&optional config &rest make-config-args)
  "start the service. `config' is used as the base service configuration
any remaining arguments are interpreted as flattened key-value pairs and are proxied to
`make-config-args'
"
  (when (service-running-p *service*)
    (error "service is running"))
  (setf config (merge-structs 'config
                              config
                              (apply 'make-config make-config-args)
                              config-default
                              ))
  (setf (config-ws-port config) (1+ (config-port config)))
  (apply 'tetris-ai:init-tetris
         (append (when (config-shapes-file config)
                   (list :shapes-file (config-shapes-file config)))
                 (when (config-seed config)
                   (list :seed (config-seed config)))))

  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :port (config-port config)
                                 :document-root (truename "./www")
                                 :access-log-destination nil))
        (ws-acceptor (make-instance 'hunchensocket:websocket-acceptor
                                    :port (config-ws-port config))))
    ;; TODO use the same port
    ;; https://github.com/joaotavora/hunchensocket/issues/14
    (hunchentoot:start acceptor)
    (hunchentoot:start ws-acceptor)

    (vom:warn "started on port ~D (ws ~D)" (config-port config) (config-ws-port config))
    (setf *service*
          (make-service :config (or config config-default)
                        :acceptor acceptor
                        :game-executions (make-hash-table)
                        :ws-acceptor ws-acceptor
                        :curr-game-no 0))))

(defun service-stop (&optional service)
  "stop the service if running. if service is nil, stop *service*"
  (when (setf service (or service *service*))
    (let* (;; (acceptor (service-acceptor service))
           (acceptor (slot-value service 'acceptor)))
      (when (and acceptor (hunchentoot:started-p acceptor))
        (hunchentoot:stop acceptor)))
    (loop for thread in (sb-thread:list-all-threads)
       if (and thread (s-starts-with thread-name-prefix (sb-thread:thread-name thread)))
       do
         (sb-thread:terminate-thread thread))))

(defun service-running-p (&optional service)
  "return whether service is currently running"
  (unless service (setf service *service*))
  (and service (slot-value service 'acceptor)
       (hunchentoot:started-p (slot-value service 'acceptor))))

(defmacro define-regexp-route (name (url-regexp &rest capture-names) docstring &body body)
  "a macro to define a handler `name' matching requests for `url-regexp'.
An optional list `capture-names' can be provided to capture path variables.
The capturing behavior is based on wrapping `ppcre:register-groups-bind'
"
  `(progn
     (defun ,name ()
       ,docstring
       (ppcre:register-groups-bind ,capture-names
           (,url-regexp (hunchentoot:script-name*))
         ,@body))
     (push (hunchentoot:create-regex-dispatcher ,url-regexp ',name)
           hunchentoot:*dispatch-table*)))

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/" "./www/index.html")
      hunchentoot:*dispatch-table*)

(define-regexp-route current-game-state-handler ("^/games/([0-9]+)/?$"
                                                 (#'parse-integer game-no))
  "return the current state of the game `game-no'"
  (let* ((game-exc (gethash game-no (service-game-executions *service*))))
    (if (null game-exc)
        (json-resp hunchentoot:+HTTP-NOT-FOUND+
                   '(:error "no such game"))
        (json-resp nil game-exc))))

(defun game-exc-move (game-exc move-no &aux moves)
  (setf moves (game-execution-moves game-exc))
  (cond
    ((and (not (game-execution-running-p game-exc)) (>= move-no (length moves)))
     (values hunchentoot:+HTTP-REQUESTED-RANGE-NOT-SATISFIABLE+
             '(:error "requested move outside of range of completed game")))
    (t (loop with
          max-move-catchup-wait-secs = (config-max-move-catchup-wait-secs
                                        (service-config *service*))
          for i below max-move-catchup-wait-secs
          as behind = (>= move-no (length moves))
          while behind
          do (progn
               (vom:debug "catching up from ~D to ~D (~D secs left)~%"
                          (length moves) move-no (- max-move-catchup-wait-secs i))
               (sleep 1))
          finally
            (return
              (if behind
                  (values hunchentoot:+HTTP-SERVICE-UNAVAILABLE+
                          '(:error "reached timeout catching up to requested move" ))
                  (values hunchentoot:+HTTP-OK+ (aref moves move-no))))))))

(define-regexp-route game-move-handler ("^/games/([0-9]+)/moves/([0-9]+)$"
                                        (#'parse-integer game-no) (#'parse-integer move-no))
  "return the move number `move-no' of the game number `game-no'"
  (let* ((game-exc (gethash game-no (service-game-executions *service*))))
    (if (null game-exc)
        (json-resp hunchentoot:+HTTP-NOT-FOUND+ '(:error "no such game"))
        (multiple-value-bind (ret-code data) (game-exc-move game-exc move-no)
          (json-resp ret-code data)))))

(define-regexp-route game-list-handler ("^/games/?$")
  "return a list of all existing games"
  (json-resp nil
             (loop for game-no being the hash-keys of (service-game-executions *service*)
                collect game-no)))

(hunchentoot:define-easy-handler (shapes :uri "/shapes") ()
  "return the shape configurations used by the service"
  (tetris-ai:serialize-shapes))

(defun game-serialize-state (game move-no)
  "serialize the current state of the game `game' at move number `move-no'"
  (make-last-recorded-state
   :timestamp (get-universal-time)
   :move-no move-no
   :on-cells (tetris-ai:game-on-cells-packed game)))

(defun game-run (game-exc)
  "evaluate a game excution spec `game-exc'
until either the game is lost, or `max-moves' is reached"
  (setf (game-execution-running-p game-exc) t)
  (with-slots (game moves max-moves ai-move-delay-secs
                    last-recorded-state-check-delay-secs)
      game-exc
    (loop
       with last-recorded-state-check-multiple
         = (max 1 (floor last-recorded-state-check-delay-secs ai-move-delay-secs))
       with print-string = nil
       for i from 0
       as next-move = (tetris-ai:game-apply-next-move game)
       while (and next-move (or (null max-moves) (< i max-moves)))
       do (when (eq (cadr vom:*config*) :DEBUG4);;this should be a single (vom:debug ...) call
            (vom:debug (setf print-string (tetris-ai:game-printable-string game print-string))))
       do
         (let ((native (cffi:translate-from-foreign next-move 'tetris-ai::game-move)))
           (progn
             (unless (zerop ai-move-delay-secs)
               (sleep ai-move-delay-secs))
             (vector-push-extend native moves)))
       if (zerop (mod i last-recorded-state-check-multiple))
       do
         (setf (game-execution-last-recorded-state game-exc)
               (game-serialize-state game i))
       finally
         (setf (game-execution-running-p game-exc) nil
               (game-execution-final-state game-exc) (game-serialize-state game i)))))

(defun game-create (game-no &key max-moves ai-move-delay-secs
                              (last-recorded-state-check-delay-secs 2))
  "create a game `game-no' with the specified `max-moves', `ai-move-delay-secs',
`last-recorded-state-check-delay-secs'. service-global configs are drawn from
(service-config *service*)"
  (unless (service-running-p *service*)
    (error "service not running"))
  (assert (numberp game-no))
  (when (gethash game-no (service-game-executions *service*))
    (error "game ~D exists" game-no))

  (with-slots (ai-depth grid-height-width default-ai-move-delay-millis)
      (service-config *service*)
    (let* ((moves (make-array 0 :adjustable t
                              :fill-pointer t
                              :element-type 'tetris-ai:game-move))
           (ai-move-delay-secs (or ai-move-delay-secs
                                   (/ default-ai-move-delay-millis 1000)))
           (height-width grid-height-width)
           (game (tetris-ai:game-init (car height-width)
                                      (cdr height-width)
                                      :ai-weights tetris-ai:ai-default-weights
                                      :ai-depth ai-depth)))
      (assert (> ai-move-delay-secs 0))
      (assert (> last-recorded-state-check-delay-secs 0))
      (setf (gethash game-no (service-game-executions *service*))
            (make-game-execution :game game
                                 :moves moves
                                 :max-moves max-moves
                                 :last-recorded-state (game-serialize-state game 0)
                                 :ai-move-delay-secs ai-move-delay-secs
                                 :last-recorded-state-check-delay-secs
                                 last-recorded-state-check-delay-secs)))))

(defun game-create-run (&optional game-no &rest create-args)
  "create and execute a game. all arguments are proxied to `game-create'"
  (let ((game-no (or game-no (incf (service-curr-game-no *service*)))))
    (game-run (apply 'game-create game-no create-args))))

(defun game-create-run-thread (&optional game-no &rest create-args)
  "create and execute a game on a new thread. all arguments are proxied to `game-create-run'"
  (let* ((game-exc (apply 'game-create game-no create-args)))
    (values
     (sb-thread:make-thread 'game-run :arguments (list game-exc)
                            :name (format nil "~A ~D" thread-name-prefix game-no))
     game-exc)))
