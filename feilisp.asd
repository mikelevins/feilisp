;;;; feilisp.asd

(asdf:defsystem #:feilisp
  :description "Describe feilisp here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:folio2)
  :components ((:file "package")
               (:file "feilisp")))

;;; (asdf:load-system :feilisp)
