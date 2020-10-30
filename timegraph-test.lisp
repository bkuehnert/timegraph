(defpackage #:timegraph.test
  (:use #:cl
        #:clunit
        #:timegraph)
  (:export #:run))

(in-package #:timegraph.test)

(defun run (&key use-debugger)
  (run-suite 'tg-suite :use-debugger use-debugger
                       :signal-condition-on-fail t))

(defsuite tg-suite ())
