(asdf:defsystem "clplnr"
  :description "Micro-Planner in Common Lisp, translated from plnr.181 (ITS/MacLisp, Jan 1978)"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "compat")
               (:file "globals")
               (:file "planner")))
