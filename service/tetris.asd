(asdf:defsystem #:tetris
  :serial t
  :depends-on (
	       #:cffi
	       #:hunchentoot
	       #:cl-ppcre
	       #:getopt
	       ;; #:lisp-unit
	       #:jonathan
	       )
  :components (
	       (:file "libtetris")
               (:file "server")
               )
  )

(defsystem :server-test
  :description "tetris server tests package"
  :components ((:module "test"
                        :serial t
                        :components ((:file "server"))))
  :depends-on (:tetris :drakma :stefil))
