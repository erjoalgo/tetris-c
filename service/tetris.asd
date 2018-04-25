(asdf:defsystem #:tetris
  :serial t
  :depends-on (
	       #:cffi
	       #:hunchentoot
	       #:cl-ppcre
	       #:getopt
	       )
  :components (
               (:file "server")
	       (:file "libtetris")
               )
  )
