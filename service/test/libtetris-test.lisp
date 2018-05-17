(in-package #:tetris-ai)

(defun test-game ()
  (let ((game (game-init default-height default-width :ai-weights ai-default-weights)))
    (loop
       for i below 10
       as string = (game-printable-string game string)
       do
         (progn
           (game-apply-next-move game)
           (vom:warn string)
           (sleep .5)))))

(defun test-game-moves ()
  (loop for shape-code below 10
     do (loop for rot below 4 do
             (loop for col below 100
                as original = (make-game-move :shape-code shape-code
                                              :rot rot
                                              :col col)
                as back = (game-move-unpack (game-move-pack original))
                do (assert (equalp original back))))))

(defun run-tests ()
  (init-tetris)
  (test-game-moves)
  (cffi:foreign-funcall "grid_test")
  (test-game))

(run-tests)
