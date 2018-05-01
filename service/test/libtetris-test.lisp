(in-package #:tetris-ai)

(defun test-game ()
  (let ((game (game-init HEIGHT WIDTH ai-default-weights)))
    (loop
       for i below 10
       as string = (game-printable-string game string)
       do
         (progn
           (game-apply-next-move game)
           (vom:warn string)
           (sleep .5)))))

(defun run-tests ()
  (init-tetris)
  (cffi:foreign-funcall "grid_test")
  (test-game))

(run-tests)
