(in-package #:tetris-ai-rest)

(defun merge-structs (type &rest objs)
  "merge several structs.
values appearing earlier have higher precedence. nil interpreted as undefined"
  (loop with ret = (make-instance type)
     with slots = (loop for slot in (sb-mop:class-direct-slots (find-class type))
                     collect (slot-value slot 'SB-PCL::NAME))
     for obj in (reverse objs) if obj do
       (loop for slot in slots
          as val = (slot-value obj slot)
          if val do
            (setf (slot-value ret slot) val))
     finally (return ret)))


(defun s-starts-with (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix (subseq string 0 (length prefix)))))

(defun json-resp (return-code body)
  "convert a lisp object into a json response with the appropriate content type
to be called within a hunchentoot handler.
"
  (when return-code
    (setf (hunchentoot:return-code*) return-code))
  (setf (hunchentoot:content-type*) "application/json")
  (jonathan:to-json body))


(defmethod jonathan:%to-json ((game-exc game-execution))
  "define the serialization of a `game-execution'"
  (with-slots (game running-p last-recorded-state
                    ai-move-delay-secs)
      game-exc
    (jonathan:with-object
      (jonathan:write-key-value "width" (tetris-ai:game-width game))
      (jonathan:write-key-value "height" (tetris-ai:game-height game))
      (jonathan:write-key-value "running_p" (or running-p :false))
      (jonathan:write-key-value "ai-move-delay-secs" ai-move-delay-secs)
      (with-slots (move-no on-cells) (game-execution-last-recorded-state game-exc)
        (jonathan:write-key-value "move_no" move-no)
        (jonathan:write-key-value "on_cells" on-cells)))))

(defmethod jonathan:%to-json ((game-move tetris-ai:game-move))
  "define the serialization of a `game-move'"
  (with-slots (tetris-ai::shape-code tetris-ai::rot tetris-ai::col) game-move
    (jonathan:with-object
      (jonathan:write-key-value "shape" tetris-ai::shape-code)
      (jonathan:write-key-value "rot" tetris-ai::rot)
      (jonathan:write-key-value "col" tetris-ai::col))))