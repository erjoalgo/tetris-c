(in-package :tetris-ai-rest)

(defparameter *version* "SNAPSHOT")

(defparameter +service-option-spec+
  '(
    (("port" #\p) :type integer :optional t :documentation "service port")
    (("shapes-file" #\s) :type boolean :optional t :documentation "path to a libtetris shapes.in")
    (("seed" #\e) :type integer :optional t :documentation "libtetris ai seed to use")
    (("dims" #\d) :type integer :optional t :documentation
     "dimensions of the grid, in the form of HxW, e.g. 19x10")
    ;; (("ai-move-delay-secs" #\w) :type integer :optional t :documentation "delay between ai moves")
    (("verbose" #\v) :type boolean :optional t :documentation "verbose logging")
    (("help" #\h) :type boolean :optional t :documentation "display help")
    (("version" #\V) :type boolean :optional t :documentation "display version")))


(defmacro pull-some-kw-args ((syms remaining) args &body body)
  (let ((k (gensym "k"))
        (v (gensym "v"))
        (sym (gensym "sym")))
    `(let (,@syms remaining)
       (loop for (,k ,v) on ,args
          by #'cddr
          as ,sym = (find-symbol (symbol-name ,k))
          do (format t "sym ~A ~A~%" ,sym (member ,sym ',syms))
          if (member ,sym ',syms)
          do (progn (set ,sym ,v)
                    (format t "(setf ~A ~A) => ~A~%"
                            (symbol-name ,sym) ,v (symbol-value ,sym)))
          else nconc (cons ,k ,v) into ,remaining)
       (setf ,(car syms) 'test)
       ,@body)))


(defun main-parse-args (&rest args &key positional verbose dims help version &allow-other-keys)
  (declare (ignore positional))
  ;; destructure any argument that need to be handled before proxying to make-config
  (cond
    (help (command-line-arguments:show-option-help +service-option-spec+ :sort-names t))
    (version (format t "~A~%" *version*))
    (t
     (when verbose (vom:config t :debug4))
     (let ((config (apply 'make-config (append args '(:allow-other-keys t)))))
       (when dims
         (ppcre:register-groups-bind ((#'parse-integer h) (#'parse-integer w))
             ("([0-9]+)x([0-9]+)" dims)
           (setf (config-grid-dimensions config) (cons h w))))
       (service-start config))
     (loop do (game-create-run)))))

(defun main (args)
  (command-line-arguments:handle-command-line
   +service-option-spec+ 'main-parse-args
   :command-line (cdr args) ;;first is executable path
   :name "tetris-ai-rest"
   :rest-arity :positional))

'(main '("./script" "-p" "1234" "-h" "-v" "pmlkasdf"))