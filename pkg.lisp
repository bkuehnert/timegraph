(in-package "CL-USER")

(defpackage #:timegraph
  (:use)
  (:export
   #:time-prop-p
   #:assert-prop
   #:eval-prop
   #:make-timegraph
   #:*tg*

   ;; Relation symbols to export
   #:equals
   #:before
   #:after
   #:consec
   #:at-about
   #:precond-of
   #:postcond-of))

(defpackage #:timegraph.implementation
  (:use :common-lisp :timegraph))
