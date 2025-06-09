(defpackage :reversi
  (:use :cl :ltk)
  (:export :main))
(in-package :reversi)

(defun main ()
  (with-ltk ()
    (wm-title *tk* "hobby-reversi.lisp")
    (minsize *tk* 300 300)
    (let ((label (make-instance 'label
                                :text "Hello, world!!"))
          (b (make-instance 'button
                            :master nil
                            :text "EXIT"
                            :command (lambda () (setf *exit-mainloop* t)))))
      (pack label)
      (pack b))))
