;;;; -*- lisp -*-

(defsystem #:aaf
  :depends-on ()
  :components 
  ((:file "defpackage")
   (:file "graph")
   (:file "sets")
   (:file "properties")
   (:file "framework")
   (:file "generate")
   (:file "semantics")
   (:file "test")
   (:file "rc")
   (:file "construct")
   (:file "explore")
   (:file "embed")
   (:file "tex")
   (:file "basefw")
   (:file "ud3")
   (:file "realizations")
   (:file "realize3")
   )
  :serial t)

(defmethod perform :after ((op asdf:load-op)
			   (system (eql #.(asdf:find-system :aaf))))
  (in-package :aaf))
