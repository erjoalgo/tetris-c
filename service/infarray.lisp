(defpackage #:tetris-ai-rest/infarray
  (:use :cl)
  (:export
   #:infarray-new
   #:infarray-nth
   #:infarray-push)
  (:documentation "an infinite array that only remembers the last n elements"))

(in-package #:tetris-ai-rest/infarray)

(defstruct infarray
  page
  len
  page-len)

(defun infarray-new (element-type &key (max-elements 10000000))
  (assert (>= max-elements 2))
  (make-infarray
   :page-len max-elements
   :page (make-array max-elements :element-type element-type)
   :len 0))

(defun infarray-nth (infarray nth)
  (with-slots (page len page-len)
      infarray
    (if (or (< nth 0) (>= nth len))
        (values nil :out-of-bounds)
        (let ((oldest-idx (- len page-len)))
          (if (< nth oldest-idx)
              (values nil :forgotten)
              (values (aref page (mod nth page-len))))))))

(defun infarray-push (infarray elt)
  (with-slots (page page-len len)
      infarray
    (let ((idx (mod len page-len)))
      (setf (aref page idx) elt)
      (incf len))))

(defun infarray-length (infarray)
  (infarray-len infarray))
