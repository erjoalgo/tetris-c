(in-package :tetris-ai-rest)

(defparameter *version* "SNAPSHOT")

(defparameter +service-option-spec+
  '(
    (("port" #\p) :type integer :optional t :documentation "service port")
    (("shapes-file" #\s)
     :type string :optional t
     :documentation "path to a libtetris shapes.in"
     :initial-value "shapes.in")
    (("ai-weights-file" #\a) :type string :optional t :documentation "path to a libtetris ai weights file")
    (("seed" #\e) :type integer :optional t :documentation "libtetris ai seed to use")
    (("dims" #\g) :type string :optional t :documentation
     "dimensions of the grid, in the form of HxW, e.g. 19x10")
    (("ai-depth" #\d) :type integer :optional t :documentation "libtetris ai depth")
    (("default-ai-move-delay-millis" #\m) :type integer :optional t :documentation "delay between ai moves")
    (("log-filename" #\l) :type string :optional t :documentation "filename where to log connections")
    (("verbose" #\v) :type boolean :optional t :documentation "verbose logging")
    (("help" #\h) :type boolean :optional t :documentation "display help")
    (("version" #\V) :type boolean :optional t :documentation "display version"))
  "a spec for the tetris-ai-rest service cli")


(defun main-parse-args (&rest args &key positional verbose dims help version &allow-other-keys)
  "parse command-line arguments and start the service if applicable"
  (declare (ignore positional))
  ;; destructure any argument that need to be handled before proxying to make-config
  (cond
    (help (command-line-arguments:show-option-help +service-option-spec+ :sort-names t))
    (version (format t "~A~%" *version*))
    (t
     (when verbose
       (vom:config t :debug)
       (vom:debug "verbose enabled"))
     (let ((config (apply 'make-config (append args '(:allow-other-keys t)))))
       (when dims
         (ppcre:register-groups-bind ((#'parse-integer h) (#'parse-integer w))
             ("([0-9]+)x([0-9]+)" dims)
           (setf (config-grid-height-width config) (cons h w))))
       (service-start-with-config config))
     (loop do (sb-thread:join-thread (game-create-run-thread))))))

(defun main (&optional args)
  "main entry point"
  (command-line-arguments:handle-command-line
   +service-option-spec+ 'main-parse-args
   :command-line (cdr args) ;;first is executable path
   :name "tetris-ai-rest"
   :rest-arity :positional))

'(main '("./script" "-p" "1234" "-h" "-v"))
