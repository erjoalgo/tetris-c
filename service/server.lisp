(defpackage #:server
  (:use :cl)
  ;; (:use :cl :hunchentoot)
  (:export
   ;; #:file-string #:list-directory-recursive #:list-directory #:list-to-hash-table
   )
  )

(in-package #:server)

(defvar acceptor nil)

(unless acceptor
  (setf acceptor (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))))

(defvar dbg)

(defparameter *curr-gameno* 3)

(hunchentoot:define-easy-handler (game-status :uri "/game-status") ()
  (let* ((game-no *curr-gameno*)
         (move-no 0)
         (game (car (gethash game-no games)))
         resp)

    (if (null game)
        ;; TODO non-200
        "no current games"
        (progn
          (setf resp (list (libtetris:game-height game) (libtetris:game-width game) move-no game-no))
          '(libtetris:game-grid-iter game (lambda (r c v) (when (not (zerop v))
                                                            (push c resp)
                                                            (push r resp))))
          (format nil "[~{~D~^, ~}]" resp)))))

(hunchentoot:define-easy-handler (move :uri "/moves") (game-no move-no)
  ;; (setf (hunchentoot:content-type*) "text/plain")
  (let ((moves (cdr (gethash game-no games))))
    (if (null moves) "no such game" ;; TODO non-200
        (loop while (>= move-no (length moves))
           do (progn (format t "waiting for game to catch up to from ~D to ~D on game ~D~%"
                             (length moves) move-no game-no)
                     (sleep 1))
           finally
             (return (format nil "[~{~D~^, ~}]"
                             ;; TODO export
                             (with-slots (libtetris::shape-code libtetris::rot libtetris::col)
                                 (aref moves move-no)
                               (list libtetris::shape-code libtetris::rot
                                     libtetris::col))))))))

(defvar games (make-hash-table))

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

(defun game-run (game moves)
  (loop
     for i from 0
     upto 100
     as next-move = (libtetris:game-apply-next-move game)
     while next-move
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

(defun game-create-run (game-no)
  (destructuring-bind (game . moves) (game-create game-no)
      (game-run game moves)))

(defun game-create-run-thread (game-no)
  (sb-thread:make-thread 'game-create-run :arguments (list game-no)))

(format t "test curl: ~A~%" (test-curl))
