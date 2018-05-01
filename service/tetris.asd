(asdf:defsystem :tetris-ai
  :serial t
  :description "A cffi wrapper on libtetris"
  :license "BSD-2-Clause"
  :author "Ernesto Alfonso <erjoalgo@gmail.com>"
  :depends-on (
	       #:cffi
	       #:vom
	       )
  :components ((:file "libtetris")))

(asdf:defsystem :tetris-ai-rest
  :description "A restful service on top of tetris-ai"
  :license "BSD-2-Clause"
  :author "Ernesto Alfonso <erjoalgo@gmail.com>"
  :components ((:file "server"))
  :depends-on (:tetris-ai :hunchentoot :jonathan :vom))

(asdf:defsystem :tetris-ai-rest-test
  :description "tetris-ai-rest tests package"
  :license "BSD-2-Clause"
  :author "Ernesto Alfonso <erjoalgo@gmail.com>"
  :components ((:file "test/server-test"))
  :depends-on (:tetris-ai-rest :drakma :stefil))
