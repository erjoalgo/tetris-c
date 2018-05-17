(defpackage #:tetris-ai
  (:use :cl :cffi)
  (:export
   #:make-game-move-native #:game-move-native
   #:game-init #:game-width #:game-height
   #:default-height #:default-width #:default-shapes-file #:ai-default-weights
   #:game-apply-next-move
   #:game-print
   #:serialize-shapes
   #:init-tetris
   #:game-over-p
   #:cell-pack
   #:cell-unpack
   #:game-on-cells-packed
   #:game-grid-loop
   #:game-printable-string
   #:game-move
   #:game-move-pack
   #:game-move-unpack
   )
  )

(in-package #:tetris-ai)

(define-foreign-library libtetris
  (:unix (:or "libtetris.so.1" "libtetris.so"))
  (t (:default "libtetris.so")))

(use-foreign-library libtetris)

(defvar default-seed (cffi:foreign-funcall "time" :pointer (cffi:null-pointer) :int))
(defvar default-shapes-file "shapes.in")

(cffi:defcvar ("SHAPE_COUNT" shape-count) :int)
(cffi:defcvar ("SHAPES" shapes) :pointer)

(defun init-tetris (&key (seed default-seed) (shapes-file default-shapes-file))
  "initialize the tetris foreign library with the given `seed' and `shapes-file'"
  (vom:debug "reading shapes...~%" )
  (setf shapes
        (cffi:foreign-funcall "shapes_read"
                              :string shapes-file
                              :pointer (cffi:get-var-pointer 'SHAPE-COUNT)
                              :pointer))
  (vom:info "loaded ~D shapes ~%" SHAPE-COUNT)
  (vom:warn "using seed: ~D~%" seed)
  (cffi:foreign-funcall "srand" :int seed)
  (assert (> SHAPE-COUNT 0))
  (cffi:foreign-funcall "ai_init" :void))

(defstruct game
  g ;; grid
  b ;; block
  ss ;; shape-stream
  height
  width
  ai-weights
  over-p
  )

(defun game-init (height width &key ai-weights (ai-depth 3))
  "initialize a game with the given `height' and `width'"
  (make-game :g (cffi:foreign-funcall "grid_new" :int height :int width :pointer)
             :b (cffi:foreign-funcall "block_new" :pointer)
             :ss (cffi:foreign-funcall "shape_stream_new" :int ai-depth :pointer)
             :height height
             :width width
             :ai-weights ai-weights))

(defun game-cycle-next-shape (game)
  "pop the next shape in the shape stream, and position it at the top.
return a block with the newly-popped shape, or nil if the block intersects the grid"
  (with-slots (g b ss) game
    (let (next-shape)
      (setf next-shape (cffi:foreign-funcall "shape_stream_pop"
                                             :pointer ss
                                             :pointer))
      (cffi:foreign-funcall "block_init"
                            :pointer b
                            :pointer next-shape
                            :void)
      (cffi:foreign-funcall "grid_block_center_elevate"
                            :pointer g
                            :pointer b)
      (unless (cffi:foreign-funcall "grid_block_intersects"
                                    :pointer g
                                    :pointer b
                                    :boolean)
        b))))

(defun game-apply-move (game move &optional no-add)
  "apply the specified `move' to `game'"
  (cffi:foreign-funcall "grid_block_apply_move"
                        :pointer (game-g game)
                        :pointer (game-b game)
                        :pointer move
                        :int (if no-add 0 1)
                        :boolean))

(defmacro game-grid-loop (game r-sym c-sym val-sym
                          &key
                            (outer-action :do)
                            (inner-action :do)
                            (condition nil)
                            body
                            )
  "a loop-wrapper to iterate over every cell in the grid of `game'. the coordinate (r,c)
will be bound to `r-sym' and `c-sym' respectively, and the value at (r,c) will be boud to `val'"
  (let ((if-cond (when condition `(if ,condition)))
        (rows (gensym "rows"))
        (row (gensym "row")))
    `(loop
        with ,rows = (cffi:mem-ref (game-g game) :pointer 0)
        for ,r-sym downfrom (1- (game-height game)) to 0
        as ,row = (cffi:mem-aref ,rows :pointer r)
          ,outer-action
          (loop for ,c-sym below (game-width ,game)
             as ,val-sym = (cffi:mem-aref ,row :int c)
               ,@if-cond
               ,inner-action
               ,body))))


(defun cell-pack (r c width)
  "pack an (r,c) into a single number according to a given `width'"
  (+ (* r width) c))

(defun cell-unpack (rc width)
  "unpack a value packed by `cell-pack' back  into a (r, c), returned as multiple values"
  (values (floor rc width)
          (mod rc width)))

(defun game-on-cells-packed (game &aux width)
  "return a list of those cells whose value is non-zero, packed by `cell-pack'"
  (setf width (game-width game))
  (game-grid-loop game r c val
                  :outer-action nconc
                  :inner-action collect
                  :condition (not (zerop val))
                  :body (cell-pack r c width)))

(defun game-printable-string (game &optional string)
  "write a printable representation of the game into `string', or, if nil, into a new string"
  (let* ((swidth (1+ (game-width game)))
         (height (game-height game)))
    (unless nil
      (setf string (make-string (* (game-height game) swidth)
                                :initial-element #\Newline)))
    (game-grid-loop game r c val
                    :body (setf (aref string (cell-pack (- height 1 r) c swidth))
                                (if (zerop val) #\Space #\*)))
    string))

(defun game-move-pack (game-move)
  ;; TODO switch to bit ops
  (with-slots (shape-code rot col) game-move
    (+ (ash shape-code 16)
       (ash rot 8)
       (ash col 0))))

(defun game-move-unpack (packed &optional game-move)
  (unless game-move (setf game-move (make-game-move)))
  (let* ((MASK #xff)
         (shape-code (logand MASK (ash packed -16)))
         (rot (logand MASK (ash packed -8)))
         (col (logand MASK (ash packed 0))))
    (setf (game-move-shape-code game-move) shape-code
          (game-move-rot game-move) rot
          (game-move-col game-move) col)
    game-move))

(defvar default-height 19)
(defvar default-width 10)

(cffi:defcvar ("default_weights" ai-default-weights) :pointer)

(defun ai-best-move (game weights)
  "return the best move according to the ai"
  (cffi:foreign-funcall "ai_best_move"
                        :pointer (game-g game)
                        :pointer (game-ss game)
                        :pointer weights
                        :pointer))

(defstruct game-move shape-code rot col)

(defcstruct (%game-move-foreign :class game-move-foreign)
  "game move"
  (shape :pointer)
  (rot :int)
  (col :int))

(defmethod translate-from-foreign (pointer game-move)
  "translate a C foreign move into a `game-move'"
  (declare (ignore game-move))
  (with-foreign-slots ((shape rot col) pointer (:struct %game-move-foreign))
    (let ((shape-id (mem-ref shape :int)))
      (make-game-move :shape-code shape-id :rot rot :col col))))

(defun game-apply-next-move (game &optional game-move)
  "sequence of game-cycle-next-shape, ai-best-move, and game-apply-move"
  (unless game-move
    ;; TODO allow specifying move
    ;; ensure shapes match
    )
  (let* ((next-move (ai-best-move game (game-ai-weights game))))
    (game-cycle-next-shape game)
    (when (game-apply-move game next-move) next-move)))

(defun serialize-shape (shape-ptr)
  "serialize shape into a json format"
  (cffi:foreign-funcall "shape_serialize"
                        :pointer shape-ptr
                        :string))

(defun serialize-shapes ()
  "serialize all the shapes currently in use"
  (format nil "[~%~{~a~^,~%~}~%]~%"
          (loop for id below SHAPE-COUNT
             collect (serialize-shape (mem-aref SHAPES :pointer id)))))

;; (test-game)

