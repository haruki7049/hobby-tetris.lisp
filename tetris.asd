(defsystem "tetris"
  :version "0.1.0"
  :depends-on ("cl-tui"
               "arrow-macros")
  :pathname "src"
  :components ((:file "main")))
