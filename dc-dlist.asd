;; dc-dlist.asd

(asdf:defsystem #:dc-dlist
  :description "Doubly linked list."
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :serial t
  :components ((:file "dc-dlist-package")
               (:file "dc-dlist")))
