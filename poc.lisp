(ql:quickload "cffi")

(asdf:load-system :cffi)

  ;;; Nothing special about the "CFFI-USER" package.  We're just
  ;;; using it as a substitute for your own CL package.
(defpackage :cffi-user
  (:use :common-lisp :cffi))

(in-package :cffi-user)

(define-foreign-library libcurl
    (:darwin (:or "libcurl.3.dylib" "libcurl.dylib"))
  (:unix (:or "libcurl.so.3" "libcurl.so"))
  (t (:default "libcurl")))

(define-foreign-library libtetris
  (:unix (:or "libtetris.so.1" "libtetris.so"))
  (t (:default "libtetris.so")))

(use-foreign-library libtetris)

(define-foreign-library libtetris
  (:unix (:or "libtetrissss.so")))

(use-foreign-library libcurl)

  ;;; A CURLcode is the universal error code.  curl/curl.h says
  ;;; no return code will ever be removed, and new ones will be
  ;;; added to the end.
(defctype curl-code :int)

  ;;; Initialize libcurl with FLAGS.
(defcfun "curl_global_init" curl-code
  (flags :long))

(curl-global-init 0)

(defcfun "curl_easy_init" :pointer)

(defcfun "curl_easy_cleanup" :void
  (easy-handle :pointer))

(defparameter *easy-handle* (curl-easy-init))

(defmacro define-curl-options (name type-offsets &rest enum-args)
    "As with CFFI:DEFCENUM, except each of ENUM-ARGS is as follows:

      (NAME TYPE NUMBER)

  Where the arguments are as they are with the CINIT macro defined
  in curl.h, except NAME is a keyword.

  TYPE-OFFSETS is a plist of TYPEs to their integer offsets, as
  defined by the CURLOPTTYPE_LONG et al constants in curl.h."
    (flet ((enumerated-value (type offset)
             (+ (getf type-offsets type) offset)))
      `(progn
         (defcenum ,name
           ,@(loop for (name type number) in enum-args
                collect (list name (enumerated-value type number))))
         ',name)))                ;for REPL users' sanity

(define-curl-options curl-option
    (long 0 objectpoint 10000 functionpoint 20000 off-t 30000)
  (:noprogress long 43)
  (:nosignal long 99)
  (:errorbuffer objectpoint 10)
  (:url objectpoint 2))


(progn
  (defcenum curl-option
    (:noprogress 43)
    (:nosignal 99)
    (:errorbuffer 10010)
    (:url 10002))
  'curl-option)

(foreign-funcall "curl_easy_setopt"
		 :pointer *easy-handle*
		 curl-option :nosignal :long 1 curl-code)
