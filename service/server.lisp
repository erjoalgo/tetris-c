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

(defpackage #:server
  (:use :cl)
  (:export
   #:service-start
   #:service-stop
   #:game-create-run
   #:game-create-run-thread
   #:make-config
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

(defvar thread-name-prefix "tetris-game-thread")

(defun main (argv)
  (declare (ignore argv))
  ;; TODO parse args
  (let ((config config-default))
    (service-start config))
  (loop do (game-create-run)))

(defvar *service* nil)

(defun merge-structs (type &rest objs)
  "values appearing earlier have higher precedence. nil interpreted as undefined"
  (loop with ret = (make-instance type)
        with slots = (loop for slot in (sb-mop:class-direct-slots (find-class type))
                           collect (slot-value slot 'SB-PCL::NAME))
        for obj in (reverse objs) if obj do
          (loop for slot in slots
                as val = (slot-value obj slot)
                if val do
                  (setf (slot-value ret slot) val))
        finally (return ret)))

(defun service-start (&optional config &rest make-config-args)
  (when (service-running-p *service*)
    (error "service is running"))
  (setf config (merge-structs 'config
                              config
                              (apply 'make-config make-config-args)
                              config-default
                              ))
  (apply 'libtetris:init-tetris
         (append (when (config-shapes-file config)
                   (list :shapes-file (config-shapes-file config)))
                 (when (config-seed config)
                   (list :seed (config-seed config)))))

  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor
                                 :port (config-port config)
                                 :access-log-destination nil)))
    (setf (hunchentoot:acceptor-document-root acceptor) (truename "./www"))
    (hunchentoot:start acceptor)
    (setf *service*
          (make-service :config (or config config-default)
                        :acceptor acceptor
                        :game-executions (make-hash-table)
                        :curr-game-no 0))))

(defun s-starts-with (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix (subseq string 0 (length prefix)))))

(defun service-stop (&optional service)
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
  (unless service (setf service *service*))
  (and service (slot-value service 'acceptor)
       (hunchentoot:started-p (slot-value service 'acceptor))))

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

(defmethod jonathan:%to-json ((game-exc game-execution))
  (with-slots (game running-p last-recorded-state
               ai-move-delay-secs)
      game-exc
    (jonathan:with-object
      (jonathan:write-key-value "width" (libtetris:game-width game))
      (jonathan:write-key-value "height" (libtetris:game-height game))
      (jonathan:write-key-value "running_p" (or running-p :false))
      (jonathan:write-key-value "ai-move-delay-secs" ai-move-delay-secs)
      (with-slots (move-no on-cells) (game-execution-last-recorded-state game-exc)
        (jonathan:write-key-value "move_no" move-no)
        (jonathan:write-key-value "on_cells" on-cells)))))

(defmethod jonathan:%to-json ((game-move libtetris:game-move))
  (with-slots (libtetris::shape-code libtetris::rot libtetris::col) game-move
    (jonathan:with-object
      (jonathan:write-key-value "shape" libtetris::shape-code)
      (jonathan:write-key-value "rot" libtetris::rot)
      (jonathan:write-key-value "col" libtetris::col))))

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/" "./www/index.html")
      hunchentoot:*dispatch-table*)

(define-regexp-route current-game-state-handler ("^/games/([0-9]+)/?$"
                                                 (#'parse-integer game-no))
  (let* ((game-exc (gethash game-no (service-game-executions *service*))))
    (if (null game-exc)
        (json-resp hunchentoot:+HTTP-NOT-FOUND+
                   '(:error "no such game"))
        (json-resp nil game-exc))))

(define-regexp-route game-move-handler ("^/games/([0-9]+)/moves/([0-9]+)$"
                                        (#'parse-integer game-no) (#'parse-integer move-no))
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
                        (vom:debug "catching up from ~D to ~D on game ~D (~D secs left)~%"
                                (length moves) move-no game-no (- max-move-catchup-wait-secs i))
                        (sleep 1))
                   finally
                  (return
                      (if behind
                          (json-resp hunchentoot:+HTTP-SERVICE-UNAVAILABLE+
                                     '(:error "reached timeout catching up to requested move~%" ))
                            (json-resp nil
                                       (with-slots (libtetris::shape-code libtetris::rot libtetris::col)
                                           (aref moves move-no)
                                         (list libtetris::shape-code libtetris::rot
                                               libtetris::col)))))))))))

(define-regexp-route game-list-handler ("^/games/?$")
  (json-resp nil
             (loop for game-no being the hash-keys of (service-game-executions *service*)
                   collect game-no)))

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
  (with-slots (game moves max-moves ai-move-delay-secs
               last-recorded-state-check-delay-secs)
      game-exc
    (loop
      with last-recorded-state-check-multiple
        = (max 1 (floor last-recorded-state-check-delay-secs ai-move-delay-secs))
      for i from 0
      as next-move = (libtetris:game-apply-next-move game)
      while (and next-move (or (null max-moves) (< i max-moves)))
      as string = (libtetris:game-printable-string game string)
      do (vom:debug string)
      do
         (let ((native (cffi::translate-from-foreign next-move 'libtetris::game-move)))
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

(defun game-create (game-no &key max-moves (ai-move-delay-secs .5)
                              (last-recorded-state-check-delay-secs 2))
  (unless (service-running-p *service*)
    (error "service not running"))
  (assert (numberp game-no))
  (when (gethash game-no (service-game-executions *service*))
    (error "game ~D exists" game-no))
  (let ((moves (make-array 0 :adjustable t
                             :fill-pointer t
                             :element-type 'libtetris:game-move))
        (game (libtetris:game-init libtetris:HEIGHT
                                   libtetris:WIDTH
                                   libtetris:ai-default-weights)))
    (setf (gethash game-no (service-game-executions *service*))
          (make-game-execution :game game
                               :moves moves
                               :max-moves max-moves
                               :last-recorded-state (game-serialize-state game 0)
                               :ai-move-delay-secs ai-move-delay-secs
                               :last-recorded-state-check-delay-secs
                               last-recorded-state-check-delay-secs))))

(defun game-create-run (&optional game-no &rest create-args)
  (let ((game-no (or game-no (incf (service-curr-game-no *service*)))))
    (game-run (apply 'game-create game-no create-args))))

(defun game-create-run-thread (&optional game-no &rest create-args)
  (let* ((game-exc (apply 'game-create game-no create-args)))
    (values
     (sb-thread:make-thread 'game-run :arguments (list game-exc)
                                      :name (format nil "~A ~D" thread-name-prefix game-no))
     game-exc)))
