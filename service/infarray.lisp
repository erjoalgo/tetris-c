(defpackage #:tetris-ai-rest/infarray
  (:use :cl)
  (:export
   #:infarray-new
   #:infarray-nth
   #:infarray-push)
  (:documentation "an infinite array that only remembers the last n elements"))

(in-package #:tetris-ai-rest/infarray)

(defstruct infarray
  max-elements
  page-max-elements
  page
  page-len
  page-last
  page-base-idx
  forgotten-pages-count)

(defun infarray-new (element-type &key (max-elements 10000000))
  (assert (> max-elements 2))
  (let ((page-max-elements (floor max-elements 2)))
    (make-infarray
     :max-elements max-elements
     :page-max-elements page-max-elements
     :page (make-array page-max-elements :element-type element-type)
     :page-last (make-array page-max-elements :element-type element-type)
     :page-base-idx 0
     :page-len 0
     :forgotten-pages-count 0)))

(defun infarray-nth (infarray nth)
  (with-slots (page page-base-idx page-len page-last page-max-elements forgotten-pages-count)
      infarray
    (let ((idx (- nth page-base-idx)))
      (if (>= idx 0)
          (if (< idx page-len)
              (aref page idx)
              (values nil :out-of-bounds))
          (if (and (>= (+ idx page-max-elements) 0)
                   (> forgotten-pages-count 0))
              (aref page-last (+ idx page-max-elements))
              (if (< idx 0)
                  (values nil :out-of-bounds)
                  (values nil :forgotten)))))))

(defun infarray-push (infarray elt)
  (with-slots (page page-base-idx page-len page-last page-max-elements
                    forgotten-pages-count)
      infarray
    (if (< page-len page-max-elements)
        (progn (setf (aref page page-len) elt)
               (incf page-len))
        (let ((new-page page-last) (old-page page))
          (format t "rotating infarray...~%" )
          (setf page new-page
                page-last old-page)
          (incf page-base-idx page-max-elements)
          (incf forgotten-pages-count)
          (infarray-push infarray elt)))))

(defun infarray-length (infarray)
  (with-slots (page-len page-max-elements forgotten-pages-count) infarray
      (+ page-len (* forgotten-pages-count page-max-elements))))
