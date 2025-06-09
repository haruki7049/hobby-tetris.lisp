(defsystem "reversi"
  :version "0.1.0"
  :depends-on ("ltk"
               "arrow-macros")
  :pathname "src"
  :components ((:file "main")))
