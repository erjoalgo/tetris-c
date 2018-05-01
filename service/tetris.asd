(asdf:defsystem :tetris-ai
  :serial t
  :depends-on (
	       #:cffi
	       #:hunchentoot
	       #:cl-ppcre
	       #:getopt
	       #:jonathan
	       #:vom
	       )
  :components (
	       (:file "libtetris")
               (:file "server")
               )
  )

(asdf:defsystem :tetris-ai-test
  :description "tetris-ai-rest tests package"
  :components ((:module "test"
                        :serial t
                        :components ((:file "server"))))
  :depends-on (:tetris :drakma :stefil))
