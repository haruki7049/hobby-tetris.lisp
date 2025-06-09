(defpackage :tetris
  (:use :cl
	:cl-tui)
  (:export :main))
(in-package :tetris)

(defun main ()
  (with-screen ()
    (put-text :root 0 0 "Hi!!")
    (refresh)
    (read-key)))
