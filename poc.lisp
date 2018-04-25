(proclaim '(optimize (speed 0) (space 0) (debug 3)))
;; (proclaim '(optimize (debug 3)))

(ql:quickload "cffi")

(asdf:load-system :cffi)

  ;;; Nothing special about the "CFFI-USER" package.  We're just
  ;;; using it as a substitute for your own CL package.
(defpackage :cffi-user
  (:use :common-lisp :cffi))

(in-package :cffi-user)

(define-foreign-library libtetris
  (:unix (:or "libtetris.so.1" "libtetris.so"))
  (t (:default "libtetris.so")))

(use-foreign-library libtetris)
















