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

(format t "starting test...~%" )

(cffi:foreign-funcall "grid_test")

(defparameter HEIGHT 19)
(defparameter WIDTH 10)

(defvar dbg)

(cffi:with-foreign-objects
    ((g :pointer)
     (b :pointer)
     (ss :pointer)
     (next-shape :pointer)
     )

  (format t "initializing grid...~%" )
  (setf g (cffi:foreign-funcall "grid_new" :int HEIGHT :int WIDTH :pointer))

  (format t "initializing block...~%" )
  (setf b (cffi:foreign-funcall "block_new" :pointer))
  (format t "initializing ss...~%" )
  (setf ss (cffi:foreign-funcall "shape_stream_new" :int 3 :pointer))

  (format t "initializing next shape... ~A~%" ss)

  (setf next-shape (cffi:foreign-funcall "shape_stream_pop"
                                              :pointer ss
                                              :pointer))

  (format t "initializing block shape...~%" )

  (cffi:foreign-funcall "block_init"
                        :pointer b
                        :pointer next-shape
                        :void)

  (cffi:foreign-funcall "grid_block_center_elevate"
                        :pointer g
                        :pointer b)

  (if (cffi:foreign-funcall "grid_block_intersects"
                            :pointer g
                            :pointer b
                            :boolean)
      (format t "cannot plane new block~%" )
      (format t "can continue"))

  (cffi:foreign-funcall "grid_block_drop"
                        :pointer g
                        :pointer b
                        :int)

  (cffi:foreign-funcall "grid_block_add"
                        :pointer g
                        :pointer b
                        :int)
  (format t "~{~a~%~}"
          (loop
             with rows = (cffi:mem-ref g :pointer 0)
             for r downfrom (1- HEIGHT) to 0
             as row = (cffi:mem-aref rows :pointer r)
             collect
               (format nil "~{~d~}"
                       (loop for c below WIDTH
                          as val = (cffi:mem-aref row :int c)
                          do (format t "val ~D~%" val)
                          do (setf dbg val)
                          collect val))))

  )
