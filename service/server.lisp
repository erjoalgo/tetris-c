(defpackage #:server
  (:use :cl)
  ;; (:use :cl :hunchentoot)
  (:export
   ;; #:file-string #:list-directory-recursive #:list-directory #:list-to-hash-table
   )
  )

(in-package #:server)

(defvar *acceptor* nil)

(defun main (argv)
  (declare (ignore argv))
  (libtetris:init-tetris)
  ;; TODO parse args
  (server-start 4242)
  (loop
     do (game-create-run *curr-gameno*)
     do (incf *curr-gameno*)))

(defun server-start (port)
  (when *acceptor* (and (hunchentoot:started-p *acceptor*)
                      (hunchentoot:stop *acceptor*)))
  (setf *acceptor* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))))

(defvar games (make-hash-table))

(defparameter *curr-gameno* 3)

(defmacro define-regexp-route (name (url-regexp &rest capture-names) &body body)
  `(progn
     (defun ,name ()
       ;; ,@(when documentation (list documentation))
       ;; ,@declarations
       ;; (when *debug*
       ;;   (format *trace-output* "=== Calling ~A Handler ===" ',name))
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

(define-regexp-route current-game-state-handler ("^/game$")
  (let* ((game-no *curr-gameno*)
         (move-no 0)
         (game (car (gethash game-no games)))
         resp)

    (if (null game)
        (json-resp hunchentoot:+HTTP-NOT-FOUND+
                   '(:error "no current games"))
        (progn
          (setf resp (list (libtetris:game-height game) (libtetris:game-width game) move-no game-no))
          '(libtetris:game-grid-iter game (lambda (r c v) (when (not (zerop v))
                                                            (push c resp)
                                                            (push r resp))))
          (format nil "[~{~D~^, ~}]" resp)))))


(defvar *max-move-catchup-wait-secs* 10)

(define-regexp-route game-move-handler ("^/games/([0-9]+)/moves/([0-9]+)$"
                                        (#'parse-integer game-no) (#'parse-integer move-no))
  "Display the contents of the ENTRY."
  ;; (setf (hunchentoot:content-type*) "text/plain")
  (let ((game-moves (gethash game-no games)))
    (if (null game-moves)
        (progn
          (json-resp hunchentoot:+HTTP-NOT-FOUND+ '(:error "no such game")))
    (destructuring-bind (game . moves) game-moves
      (cond
        ((and (libtetris:game-over-p game) (>= move-no (length moves)))
         (json-resp hunchentoot:+HTTP-REQUESTED-RANGE-NOT-SATISFIABLE+
         '(:error "requested move outside of range of completed game")))
        (t (loop for i below *max-move-catchup-wait-secs*
           as behind = (>= move-no (length moves))
           while behind
           do (progn (format t "waiting for game to catch up to from ~D to ~D on game ~D~%"
                             (length moves) move-no game-no)
                     (sleep 1))
           finally
             (if behind
                 (json-resp hunchentoot:+HTTP-SERVICE-UNAVAILABLE+
                                   '(:error "reached timeout catching up to requested move~%" ))
                 (return
                   (json-resp nil
                             ;; TODO export
                             (with-slots (libtetris::shape-code libtetris::rot libtetris::col)
                                 (aref moves move-no)
                               (list libtetris::shape-code libtetris::rot
                                     libtetris::col))))))))))))

(define-regexp-route game-list-handler ("^/games/?$")
  "Display the contents of the ENTRY."
  (json-resp nil
   (loop for game-no being the hash-keys of games collect game-no)))

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/index.html" "index.html")
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/tetris_client.js" "tetris_client.js")
      hunchentoot:*dispatch-table*)

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/loading.gif" "loading.gif")
      hunchentoot:*dispatch-table*)

(hunchentoot:define-easy-handler (shapes :uri "/shapes") ()
  (libtetris:serialize-shapes))

(defun game-run (game moves &optional max-moves)
  (loop
     for i from 0
     as next-move = (libtetris:game-apply-next-move game)
     while (and next-move (or (null max-moves) (< i max-moves)))
     do (libtetris:game-print game)
     do
       (let ((native (libtetris:my-translate-from-foreign next-move)))
         (progn
           (format t "on move ~D, shape ~D, rot ~D, col ~D~%" i
                   (slot-value native 'libtetris::shape-code)
                   (slot-value native 'libtetris::rot)
                   (slot-value native 'libtetris::col))
           (sleep .5)
           (vector-push-extend native moves)))))

(defun game-create (game-no)
  (let ((moves (make-array 0 :adjustable t
                           :fill-pointer t
                           :element-type 'libtetris:game-move-native))
        (game (libtetris:game-init libtetris:HEIGHT libtetris:WIDTH libtetris:ai-default-weights)))
    (setf (gethash game-no games) (cons game moves))))

(defun game-create-run (game-no &optional max-moves)
  (destructuring-bind (game . moves) (game-create game-no)
      (game-run game moves max-moves)))

(defun game-create-run-thread (game-no &optional max-moves)
  (sb-thread:make-thread 'game-create-run :arguments (list game-no max-moves)))
