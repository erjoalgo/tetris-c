(asdf:defsystem :tetris-ai
  :serial t
  :description "A cffi wrapper on libtetris"
  :license "GPLv3"
  :author "Ernesto Alfonso <erjoalgo@gmail.com>"
  :depends-on (
	       #:cffi
	       #:vom
	       )
  :components ((:file "libtetris")))

(asdf:defsystem :tetris-ai-rest
  :description "A restful service on top of tetris-ai"
  :license "GPLv3"
  :author "Ernesto Alfonso <erjoalgo@gmail.com>"
  :components ((:file "server")
               (:file "ws")
               (:file "util")
               (:file "main"))
  :depends-on (:tetris-ai :hunchentoot :jonathan :vom :command-line-arguments :hunchensocket))

(asdf:defsystem :tetris-ai-rest-test
  :description "tetris-ai-rest tests package"
  :license "GPLv3"
  :author "Ernesto Alfonso <erjoalgo@gmail.com>"
  :components ((:file "test/server-test"))
  :depends-on (:tetris-ai-rest :drakma :stefil))
