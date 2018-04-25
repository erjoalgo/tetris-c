(defpackage #:libtetris
  ;; (:use  #:cl)
  ;; (:use :common-lisp :cffi)
  (:use :cl :cffi)
  (:export
   ;; #:file-string #:list-directory-recursive #:list-directory #:list-to-hash-table
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
  (assert (> SHAPE-COUNT 0)))

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
  )

(defun game-init (height width)
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
    (make-game :g g :b b :ss ss :height height :width width)
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



(defun game-exec-move (game move)
  (declare (ignore move))
  ;; TODO set block to move
  (cffi:foreign-funcall "grid_block_drop"
                        :pointer (game-g game)
                        :pointer (game-b game)
                        :int)

  (cffi:foreign-funcall "grid_block_add"
                        :pointer (game-g game)
                        :pointer (game-b game)
                        :int))

(defun game-print (game)
  (format t "~{~a~%~}"
          (loop
             with rows = (cffi:mem-ref (game-g game) :pointer 0)
             for r downfrom (1- (game-height game)) to 0
             as row = (cffi:mem-aref rows :pointer r)
             collect
               (format nil "~{~d~}"
                       (loop for c below (game-width game)
                          as val = (cffi:mem-aref row :int c)
                          do (format t "val ~D~%" val)
                          do (setf dbg val)
                          collect val)))))

(defparameter HEIGHT 19)
(defparameter WIDTH 10)

(defun test-game ()
  (let ((game (game-init HEIGHT WIDTH))
        next-shape)
    (setf next-shape (game-next-shape game))
    (game-exec-move game nil)
    (game-print game)))


(test-game)
