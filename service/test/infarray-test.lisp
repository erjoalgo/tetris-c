(defpackage #:tetris-ai-infarray-test
  ;; (:use :cl :lisp-unit)
  (:use :cl)
  (:import-from #:stefil
                #:is)
  (:import-from #:tetris-ai-rest/infarray
                #:infarray-new
                #:infarray-nth
                #:infarray-length
                #:infarray-push)
  (:export #:run-tests))

(in-package #:tetris-ai-infarray-test)

(stefil:deftest infarray-test nil
  (let* ((infarr (infarray-new (type-of 11) :max-elements 10)))
    (labels ((is-nth-ok (nth elt-exp)
               (multiple-value-bind (elt err)
                   (infarray-nth infarr nth)
                 (is (eq elt elt-exp))
                 (is (null err))))
             (is-nth-err (nth err-exp)
               (multiple-value-bind (elt err) (infarray-nth infarr nth)
                 (is (eq err err-exp))
                 (is (null elt)))))

      (is (eq 0 (infarray-length infarr)))
      (is-nth-err 0 :out-of-bounds)
      (is-nth-err -1 :out-of-bounds)
      (infarray-push infarr 0)
      (is (eq 1 (infarray-length infarr)))
      (is-nth-ok 0 0)
      (is-nth-err -1 :out-of-bounds)

      (loop for i from 1 below 10
         do (infarray-push infarr i)
         do (is (eq (1+ i) (infarray-length infarr)))
         do (loop for ii upto i do
                 (is-nth-ok ii ii)))

      (loop for i from 10 below 100
         do (infarray-push infarr i)
         do (is (eq (1+ i) (infarray-length infarr)))
         do (loop for ii upto i do
                 (if (< (- i ii) 10)
                     (is-nth-ok ii ii)
                     (is-nth-err ii :forgotten)))))))

(infarray-test)
