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
  (port 4242)
  ws-port
  shapes-file
  ai-weights-file
  seed
  (grid-height-width (cons tetris-ai:default-height
                           tetris-ai:default-width))
  (max-move-catchup-wait-secs 10)
  (ai-depth 3)
  (default-ai-move-delay-millis 5)
  (log-filename "tetris-ai-rest.log")
  )

(defstruct service
  config
  acceptor
  ws-service
  (game-executions (make-hash-table))
  log-fh)

(defstruct game-execution
  game
  (moves (make-array 0 :adjustable t
                     :fill-pointer t
                     :element-type 'tetris-ai:game-move))
  last-recorded-state
  running-p

  max-moves
  ai-move-delay-secs
  (last-recorded-state-check-delay-secs 2)
  )

(defstruct last-recorded-state
  timestamp
  on-cells
  ;; on-cells-cnt
  move-no
  )

(defvar thread-name-prefix "tetris-game-thread" "prefix for name of tetris worker threads")

(defvar *service* nil "the default currently active service")

(defun service-start (&rest make-config-args)
  "start a service based on the config obtained by proxying all arguments make-config"
  (service-start-with-config (apply 'make-config make-config-args)))

(defun service-start-with-config (config)
  "start the service based on`config'"
  (when (service-running-p *service*)
    (service-stop *service*))
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
        (ws-service (ws-start (config-ws-port config)))
        (log-fh (when (config-log-filename config)
                  (open (config-log-filename config)
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create))))
    ;; TODO use the same port
    ;; https://github.com/joaotavora/hunchensocket/issues/14
    (hunchentoot:start acceptor)

    (vom:warn "started on port ~D (ws ~D)" (config-port config) (config-ws-port config))
    (setf *service*
          (make-service :config config
                        :acceptor acceptor
                        :ws-service ws-service
                        :log-fh log-fh))))

(defun service-stop (&optional service)
  "stop the service if running. if service is nil, stop *service*"
  (when (setf service (or service *service*))
    (let* (;; (acceptor (service-acceptor service))
           (acceptor (slot-value service 'acceptor))
           (ws-service (slot-value service 'ws-service)))
      (when (and acceptor (hunchentoot:started-p acceptor))
        (hunchentoot:stop acceptor)
        (ws-stop ws-service)))
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
    ((< move-no (length moves)) ;; test this first, even if redundant
     (values hunchentoot:+HTTP-OK+ (aref moves move-no)))

    ((not (game-execution-running-p game-exc))
     (values hunchentoot:+HTTP-REQUESTED-RANGE-NOT-SATISFIABLE+
             '(:error "requested move outside of range of completed game")))

    (t
     (loop with
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
         = (max 1 (floor last-recorded-state-check-delay-secs (max .001 ai-move-delay-secs)))
       with print-string = nil
       for i from 0
       as next-move = (tetris-ai:game-apply-next-move game)
       while (and next-move (or (null max-moves) (< i max-moves)))
       do (when (eq (cadr vom:*config*) :DEBUG4);;this should be a single (vom:debug ...) call
            (vom:debug4 (setf print-string (tetris-ai:game-printable-string game print-string))))
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
               (game-execution-last-recorded-state game-exc) (game-serialize-state game i)))))

(defun game-create (&rest make-game-exc-extra-args)
  "create a game `game-no' with the specified `max-moves', `ai-move-delay-secs',
  `last-recorded-state-check-delay-secs'. service-global configs are drawn from
  (service-config *service*)"

  (unless (service-running-p *service*)
    (error "service not running"))

  (with-slots (ai-depth grid-height-width default-ai-move-delay-millis ai-weights-file)
      (service-config *service*)
    (let* ((game (destructuring-bind (height . width)
                     grid-height-width
                   (tetris-ai:game-init height width
                                        :ai-weights (if ai-weights-file
                                                        (tetris-ai:load-weights ai-weights-file)
                                                        tetris-ai:ai-default-weights)
                                        :ai-depth ai-depth)))
           (game-exc (apply 'make-game-execution
                            :game game
                            :last-recorded-state (game-serialize-state game 0)

                            make-game-exc-extra-args))
           (exc-table (service-game-executions *service*))
           (game-no (HASH-TABLE-SIZE exc-table)))

      (assert (service-game-executions *service*))

      (loop while (gethash game-no exc-table)
         do (incf game-no))

      (unless (game-execution-ai-move-delay-secs game-exc)
        (setf (game-execution-ai-move-delay-secs game-exc)
              (/ default-ai-move-delay-millis 1000)))

      (setf (gethash game-no exc-table) game-exc)
      (ws-register-game game-no (service-ws-service *service*))
      (values game-exc game-no))))

(defun game-create-run-thread (&rest game-create-args)
  "create and execute a game on a new thread. all arguments are proxied to `game-create-run'"
  (multiple-value-bind (game-exc game-no) (apply 'game-create game-create-args)
    (let* ((thread-name (format nil "~A ~D" thread-name-prefix game-no)))
      (values (sb-thread:make-thread 'game-run :arguments (list game-exc)
                                     :name thread-name)
              game-exc))))
