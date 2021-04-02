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

(deftest simple-test (tg-suite)
  (let ((tg (make-timegraph)))
    (assert-prop '(e1 :before e2) tg)
    (assert-true (eval-prop '(e1 :before e2) tg))))

(deftest simple-equal-test (tg-suite)
  (let ((tg (make-timegraph)))
    (assert-prop '(e1 :before e2) tg)
    (assert-prop '(e1 :equal e2) tg)
    (assert-true (eval-prop '(e1 :equal e2) tg))))

(deftest simple-equal-test2 (tg-suite)
 (let ((tg (make-timegraph)))
   (assert-prop '(e1 :before e2) tg)
   (assert-prop '(e2 :before e3) tg)
   (assert-prop '(e1 :equal e3) tg)
   (assert-true (eval-prop '(e1 :equal e3) tg)))) 

(deftest consec-equal (tg-suite)
 (let ((tg (make-timegraph)))
   (assert-prop '(e1 :consec e2) tg)
   (assert-prop '(e2 :consec e3) tg)
   (assert-prop '(e1 :equal e3) tg)
   ;; everything collapses to a single point
   (assert-true (eval-prop '(e1 :equal e2) tg))
   (assert-true (eval-prop '(e2 :equal e3) tg))
   (assert-true (eval-prop '(e1 :equal e3) tg))))

(deftest simple-quant-test (tg-suite)
  (let ((tg (make-timegraph)))
    (assert-prop '(e1 :before e2) tg)
    (update-upper-bound 'e1 (local-time:make-timestamp :day 1) tg)
    (update-lower-bound 'e2 (local-time:make-timestamp :day 2) tg)
    (assert-true (eval-prop '(e1 :precond e2) tg))))
