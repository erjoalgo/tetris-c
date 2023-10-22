;; first call "wsPerf(10 * 1000, 1);" from js client to load the ws server

(declaim (optimize speed))
(require :sb-sprof)

(defun profile (&key (secs 20) (out "prof.out.tmp"))
  "first start the service, then call wsPerf from the client pointing to the service ws"
  (sb-sprof:reset)
  (sb-sprof:start-profiling)
  (sleep secs)
  (sb-sprof:stop-profiling)
  (sb-sprof:report :type :GRAPH :STREAM
                   (open out
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)))
