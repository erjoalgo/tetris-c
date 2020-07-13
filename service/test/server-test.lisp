(defpackage #:tetris-ai-rest-test
  ;; (:use :cl :lisp-unit)
  (:use :cl :stefil)
  (:export
   #:run-tests))

(in-package #:tetris-ai-rest-test)

(declaim (optimize (debug 3) (speed 0)))

(defvar test-game-no 5)
(defvar max-no-moves 3)
(defvar game-width 10)
(defvar game-height 19)

(defparameter test-service-config
  (tetris-ai-rest:make-config
   :port 4243
   ;; shapes-file
   ;; seed
   :grid-height-width (cons game-height game-width)
   :max-move-catchup-wait-secs 1))

(defparameter base-url (format nil "http://localhost:~D"
                               (tetris-ai-rest::config-port test-service-config)))

(defvar *test-service* nil)


;; (stefil:in-root-suite)
;; (setf (find-test 'my-test) nil)

(stefil:defsuite* test-handlers)

(defun init ()
  (when *test-service*
    (tetris-ai-rest:service-stop *test-service*))
  (tetris-ai-rest:service-stop) ;;TODO support multiple services
  (setf *test-service*
        (tetris-ai-rest:service-start test-service-config))
  (tetris-ai-rest:game-create-run test-game-no :max-moves max-no-moves)
  ;;TODO
  (setf (tetris-ai-rest::service-curr-game-no *test-service*) test-game-no))

;; https://sites.google.com/site/sabraonthehill/home/json-libraries
;; hack to return json as a string
(let ((application-json (cons "application" "json")))
  (unless (member application-json drakma:*text-content-types* :test 'equal)
    (push application-json drakma:*text-content-types*)))

(defun req (uri &key (as :alist))
  (let* ((resp (apply 'drakma:http-request (concatenate 'string base-url uri) nil))
         parsed)
    (format t "drakma req: ~A => ~A~%" uri resp)
    (setf parsed (jonathan:parse resp :as as))
    (format t "drakma req: ~A => ~A~%" uri parsed)
    parsed))

(defun json-get (key map)
  (cdr (assoc key map :test 'equal)))

(stefil:deftest test-games nil
  (let ((gameno-list (req "/games" :as :list)))
    (stefil:is (equal 1 (length gameno-list)))
    (stefil:is (equal test-game-no (nth 0 gameno-list)))))

(defvar *shape-count* nil)

(stefil:deftest test-shapes nil
  (let ((shapes (req "/shapes" :as :list)))
    (stefil:is (>= (length shapes) 0))
    (setf *shape-count* (length shapes))))

(stefil:deftest test-game-move-success nil
  (let ((game-move (req (format nil "/games/~D/moves/~D" test-game-no 0))))
    (stefil:is (= 3 (length game-move)))
    (stefil:is (loop for (k . v) in game-move always (>= v 0)))
    (stefil:is (< (json-get "shape" game-move) *shape-count*))
    (stefil:is (< (json-get "rot" game-move) 4))
    (stefil:is (< (json-get "col" game-move) game-width))
    ))

(stefil:deftest test-game-move-outside-of-range nil
  (multiple-value-bind (resp return-code)
      (drakma:http-request (format nil "~A/games/~D/moves/~D"
                                   base-url test-game-no max-no-moves))
    (declare (ignore resp))
    (stefil:is (equal hunchentoot:+HTTP-REQUESTED-RANGE-NOT-SATISFIABLE+ return-code))))

(stefil:deftest test-game-move-timeout nil
  (let ((slow-game-no (1+ test-game-no)))
    (tetris-ai-rest:game-create-run-thread slow-game-no :max-moves max-no-moves
                                           :AI-MOVE-DELAY-SECS 99999)
    (multiple-value-bind (resp return-code)
        (drakma:http-request (format nil "~A/games/~D/moves/1"
                                     base-url slow-game-no))
      (declare (ignore resp))
      (stefil:is (equal hunchentoot:+HTTP-SERVICE-UNAVAILABLE+ return-code)))))

(stefil:deftest test-new-game-serialize nil
  (let* ((new-game (tetris-ai:game-init 10
                                        10))
         (last-state (tetris-ai-rest:game-serialize-state new-game 0)))
    (is (null (tetris-ai-rest::game-state-snapshot-on-cells last-state)))))

(stefil:deftest test-game-status nil
  (let* ((game-status (req (format nil "/games/~D" test-game-no)))
         (config (tetris-ai-rest:service-config *test-service*))
         (hw (tetris-ai-rest:config-grid-height-width config)))
    (stefil:is (>= (json-get "move_no" game-status) 0))
    (stefil:is (= (cdr hw) (json-get "width" game-status) ))
    (stefil:is (= (car hw) (json-get "height" game-status)))
    (stefil:is (not (json-get "running_p" game-status)))
    ;; (stefil:is (eq t (json-get "ai-move-delay-secs" game-status)))

    ))




(defun run-tests ()
  (init)
  (test-handlers)
  (tetris-ai-rest:service-stop *test-service*))
