(defpackage #:libtetris
  ;; (:use  #:cl)
  ;; (:use :common-lisp :cffi)
  (:use :cl :cffi)
  (:export
   #:make-game-move-native #:game-move-native
   #:game-grid-iter
   #:game-init #:game-width #:game-height
   #:HEIGHT #:WIDTH #:ai-default-weights
   #:game-apply-next-move
   #:my-translate-from-foreign
   #:game-print
   #:serialize-shapes
   )
  )

(in-package #:libtetris)

(define-foreign-library libtetris
  (:unix (:or "libtetris.so.1" "libtetris.so"))
  (t (:default "libtetris.so")))

(use-foreign-library libtetris)

(defparameter seed (cffi:foreign-funcall "time" :pointer (cffi:null-pointer) :int))
(defparameter shapes-file "shapes.in")

(cffi:defcvar ("SHAPE_COUNT" shape-count) :int)
(cffi:defcvar ("SHAPES" shapes) :pointer)

(defun init-tetris (seed shapes-file)
  (format t "reading shapes...~%" )
  (setf shapes
        (cffi:foreign-funcall "shapes_read"
                              :string shapes-file
                              :pointer (cffi:get-var-pointer 'SHAPE-COUNT)
                              :pointer))
  (format t "loaded ~D shapes ~%" SHAPE-COUNT)
  (cffi:foreign-funcall "srand" :int seed)
  (assert (> SHAPE-COUNT 0))
  (cffi:foreign-funcall "ai_init" :void))

(init-tetris seed shapes-file)

(defun grid-test ()
  (format t "starting test...~%" )
  (cffi:foreign-funcall "grid_test"))

(defvar dbg)

(defstruct game
  g ;; grid
  b ;; block
  ss ;; shape-stream
  height
  width
  ai-weights
  )

(defun game-init (height width &optional ai-weights)
  (cffi:with-foreign-objects
      ((g :pointer)
       (b :pointer)
       (ss :pointer)
       )

    (format t "initializing grid...~%" )
    (setf g (cffi:foreign-funcall "grid_new" :int height :int width :pointer))

    (format t "initializing block...~%" )
    (setf b (cffi:foreign-funcall "block_new" :pointer))

    (format t "initializing ss...~%" )
    (setf ss (cffi:foreign-funcall "shape_stream_new" :int 3 :pointer))
    (make-game :g g :b b :ss ss :height height :width width :ai-weights ai-weights)
    ))

(defun game-next-shape (game)
  (let* ((ss (game-ss game))
         (next-shape (cffi:foreign-funcall "shape_stream_pop"
                                           :pointer ss
                                           :pointer)))


    (format t "initializing block shape...~%" )

    (setf next-shape (cffi:foreign-funcall "block_init"
                                           :pointer (game-b game)
                                           :pointer next-shape
                                           :void))

    (cffi:foreign-funcall "grid_block_center_elevate"
                          :pointer (game-g game)
                          :pointer (game-b game))

    ;; return whether new block could be successfully placed
    (if (not (cffi:foreign-funcall "grid_block_intersects"
                                   :pointer (game-g game)
                                   :pointer (game-b game)
                                   :boolean))
        next-shape
        nil;; game over
        )))




(defun game-apply-move (game move &optional no-add)
  (cffi:foreign-funcall "grid_block_apply_move"
                        :pointer (game-g game)
                        :pointer (game-b game)
                        :pointer move
                        :int (if no-add 0 1)
                        :boolean))

(defun game-grid-iter (game fun)
  (loop
     with rows = (cffi:mem-ref (game-g game) :pointer 0)
     for r downfrom (1- (game-height game)) to 0
     as row = (cffi:mem-aref rows :pointer r)
     do
       (loop for c below (game-width game)
          as val = (cffi:mem-aref row :int c)
          do (funcall fun r c val))))

(defun game-print (game)
  (let ((string (make-string (* (game-height game) (1+ (game-width game)))
                             :initial-element #\Newline))
        (i -1))
    (game-grid-iter game (lambda (r c v)
                           (declare (ignore r))
                           (when (and (> i -1) (zerop c)) (incf i));; skip newline
                           (setf (aref string (incf i)) (cond
                                                          ((null c) #\Newline)
                                                          ((zerop v) #\Space)
                                                          (t #\*)))))
    (format t "~A~%" string)))

(defparameter HEIGHT 19)
(defparameter WIDTH 10)

(cffi:defcvar ("default_weights" ai-default-weights) :pointer)

(defun ai-best-move (game weights)
  (cffi:foreign-funcall "ai_best_move"
                        :pointer (game-g game)
                        :pointer (game-ss game)
                        :pointer weights
                        :pointer))

(defstruct game-move-native shape-code rot col)

(defcstruct (%game-move :class game-move)
  "game move"
  (shape :pointer)
  (rot :int)
  (col :int))

;; (defmethod translate-from-foreign (pointer (type game-move))
;; TODO use proper defmethod
(defun my-translate-from-foreign (pointer &optional game-move)
  (declare (ignore game-move))
  (format t "translating...~%" )
  (with-foreign-slots ((shape rot col) pointer (:struct %game-move))
    ;; You can change this and get return value in other format
    ;; for example: (values width height)
    (let ((shape-id (mem-ref shape :int)))
      (make-game-move-native :shape-code shape-id :rot rot :col col))))


(defun game-apply-next-move (game &optional game-move)
  (unless game-move
    ;; TODO allow specifying move
    )
  (let* ((next-move (ai-best-move game (game-ai-weights game))))
    (game-next-shape game)
    (when (game-apply-move game next-move) next-move)))

(defun test-game ()
  (let ((game (game-init HEIGHT WIDTH ai-default-weights)))
    (loop for i below 10 do
         (progn
           (game-apply-next-move game)
           (game-print game)
           (sleep .5))
         )))

(test-game)
