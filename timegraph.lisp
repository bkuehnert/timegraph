(in-package #:timegraph.implementation)
;;; A timegraph is a pair of hashtables. The first hashtable resolves an
;;; episode symbol to the timepoint representing the start of the
;;; episode. Similarly, the second hashtable resolves an episode symbol
;;; to the timepoint representing the end of the episode.
(defstruct (timegraph (:conc-name tg-))
           (beg (make-hash-table :test #'equal) :type hash-table)
           (end (make-hash-table :test #'equal) :type hash-table))

;; Accessor functions
(defun get-beg (tg e1)
  (gethash e1 (tg-beg tg)))

(defun get-end (tg e1)
  (gethash e1 (tg-end tg)))

(defun set-beg (tg e1 tp)
  (pushnew e1 (tp-brefs tp))
  (setf (gethash e1 (tg-beg tg)) tp))

(defun set-end (tg e1 tp)
  (pushnew e1 (tp-erefs tp))
  (setf (gethash e1 (tg-end tg)) tp))

;; Global timegraph to be used by default.
(defvar *tg* (make-timegraph))

;; A timegraph proposition is in the form (E1 REL E2) where E1 and E2 are symbols representing
;; episodes, and REL is a keyword representing a relation. The available relations, and their
;; assertions on the timegraph are the following:
;;
;; :EQUAL - e1 and e2 start and end at the same time.
;; :BEFORE - e1 starts before e2 starts.
;; :AFTER - e1 starts after e2 starts.
;; :PRECOND - e1 ends before e2 starts.
;; :POSTCOND - e1 starts after e2 ends.
;; :DURING - e1 starts after e2 starts, and e1 ends before e2 ends.
(defun time-prop-p (prop)
  (and 
   (listp prop)
   (member (second prop) '(:equal :before :after :precond :postcond :consec :during))
   (equal (list-length prop) 3)
   t)) ; t here so that we return a boolean.

;; Asserts the proposition PROP holds in timegraph TG. By default, the global timegraph
;; is used.
(defun assert-prop (prop &optional (tg *tg*))
  (assert (time-prop-p prop))
  (let ((e1 (first prop))
        (e2 (third prop)))
    (ecase (second prop)
      (:equal (tg-assert-equal e1 e2 tg))
      (:before (tg-assert-before e1 e2 tg))
      (:after (tg-assert-before e2 e1 tg))
      (:precond (tg-assert-precond e1 e2 tg))
      (:postcond (tg-assert-precond e2 e1 tg))
      (:consec (tg-assert-consec e1 e2 tg))
      (:during (tg-assert-during e1 e2 tg)))))

;; Evaluates the proposition PROP in timegraph TG. Returns 1 if true, 0 if false,
;; and 2 if indeterminate. By default, teh global timegraph is used.
(defun eval-prop (prop &optional (tg *tg*))
  (assert (time-prop-p prop))
  (let ((e1 (first prop))
        (e2 (third prop)))
    (ecase (second prop)
      (:equal (eval-equal e1 e2 tg))
      (:before (eval-before e1 e2 tg))
      (:after (eval-before e2 e1 tg))
      (:precond (eval-precond e1 e2 tg))
      (:postcond (eval-precond e2 e1 tg))
      (:consec (eval-consec e1 e2 tg))
      (:during (eval-during e1 e2 tg)))))

;; Pre-defined functions to express episodic relationships on timepoints
;; The below functions can be automatically generated, but I haven't been able to get
;; it working. So, this will have to do for now.

(defun tg-assert-before (e1 e2 &optional (tg *tg*))
  (when (or (tp-not-before-p (get-beg tg e1) (get-end tg e1))
            (tp-not-before-p (get-beg tg e1) (get-beg tg e2))
            (tp-not-before-p (get-beg tg e2) (get-end tg e2)))
    (format t "~A before ~A is inconsistent in timegraph ~A" e1 e2 tg)
    (return-from tg-assert-before  nil))
  (let* ((pair1 (tp-assert-before tg (get-beg tg e1) (get-end tg e1)))
         (pair2 (tp-assert-before tg (first pair1) (get-beg tg e2)))
         (pair3 (tp-assert-before tg (second pair2) (get-end tg e2))))
    (set-beg tg e1 (first pair2))
    (set-end tg e1 (second pair1))
    (set-beg tg e2 (first pair3))
    (set-end tg e2 (second pair3))))

(defun tg-assert-consec (e1 e2 &optional (tg *tg*))
  (when (or (tp-not-before-p (get-beg tg e1) (get-end tg e1))
            (tp-not-equal-p (get-end tg e1) (get-beg tg e2))
            (tp-not-before-p (get-beg tg e2) (get-end tg e2)))
    (format t "~A consec ~A is inconsistent in timegraph ~A" e1 e2 tg)
    (return-from tg-assert-consec  nil))
  (let* ((pair1 (tp-assert-before tg (get-beg tg e1) (get-end tg e1)))
         (pair2 (tp-assert-equal tg (second pair1) (get-beg tg e2)))
         (pair3 (tp-assert-before tg (second pair2) (get-end tg e2))))
    (set-beg tg e1 (first pair1))
    (set-end tg e1 (first pair2))
    (set-beg tg e2 (first pair3))
    (set-end tg e2 (second pair3))))

(defun tg-assert-equal (e1 e2 &optional (tg *tg*))
  (when (or (tp-not-equal-p (get-beg tg e1) (get-end tg e1))
            (tp-not-equal-p (get-beg tg e2) (get-end tg e2)))
    (format t "~A equal ~A is inconsistent in timegraph ~A" e1 e2 tg)
    (return-from tg-assert-equal  nil))
  (let* ((pair1 (tp-assert-before tg (get-beg tg e1) (get-end tg e1)))
         (pair2 (tp-assert-equal tg (first pair1) (get-beg tg e2)))
         (pair3 (tp-assert-equal tg (second pair1) (get-end tg e2))))
    (set-beg tg e1 (first pair2))
    (set-end tg e1 (first pair3))
    (set-beg tg e2 (second pair2))
    (set-end tg e2 (second pair3))))

(defun tg-assert-precond (e1 e2 &optional (tg *tg*))
  (when (or (tp-not-before-p (get-beg tg e1) (get-end tg e1))
            (tp-not-before-p (get-end tg e1) (get-beg tg e2))
            (tp-not-before-p (get-beg tg e2) (get-end tg e2)))
    (format t "~A precond ~A is inconsistent in timegraph ~A" e1 e2 tg)
    (return-from tg-assert-precond  nil))
  (let* ((pair1 (tp-assert-before tg (get-beg tg e1) (get-end tg e1)))
         (pair2 (tp-assert-before tg (first pair1) (get-beg tg e2)))
         (pair3 (tp-assert-equal tg (second pair1) (get-end tg e2))))
    (set-beg tg e1 (first pair2))
    (set-end tg e1 (first pair3))
    (set-beg tg e2 (second pair2))
    (set-end tg e2 (second pair3))))

(defun tg-assert-during (e1 e2 &optional (tg *tg*))
  (when (or (tp-not-before-p (get-beg tg e1) (get-end tg e1))
            (tp-not-before-p (get-beg tg e2) (get-end tg e2))
            (tp-not-before-p (get-beg tg e2) (get-beg tg e1))
            (tp-not-before-p (get-end tg e1) (get-end tg e2)))
    (format t "~A during ~A is inconsistent in timegraph ~A" e1 e2 tg)
    (return-from tg-assert-during  nil))
  (let* ((pair1 (tp-assert-before tg (get-beg tg e1) (get-end tg e1)))
         (pair2 (tp-assert-before tg (get-beg tg e2) (get-end tg e2)))
         (pair3 (tp-assert-before tg (first pair2) (first pair1)))
         (pair4 (tp-assert-before tg (second pair1) (second pair2))))
    (set-beg tg e1 (second pair3))
    (set-end tg e1 (first pair4))
    (set-beg tg e2 (first pair2))
    (set-end tg e2 (second pair4))))

;; Pre-defined functions to evaluate epeisodic  relationships on timepoints
(defun eval-before (e1 e2 &optional (tg *tg*))
  (let ((t1 (get-beg tg e1))
        (t2 (get-end tg e1))
        (t3 (get-beg tg e2))
        (t4 (get-end tg e2)))
    (and (tp-before-p t1 t2) (tp-before-p t3 t4) (tp-before-p t1 t3))))

(defun eval-consec (e1 e2 &optional (tg *tg*))
  (let ((t1 (get-beg tg e1))
        (t2 (get-end tg e1))
        (t3 (get-beg tg e2))
        (t4 (get-end tg e2)))
    (and (tp-before-p t1 t2) (tp-before-p t3 t4) (tp-equal-p t1 t3))))

(defun eval-equal (e1 e2 &optional (tg *tg*))
  (let ((t1 (get-beg tg e1))
        (t2 (get-end tg e1))
        (t3 (get-beg tg e2))
        (t4 (get-end tg e2)))
    (and (tp-before-p t1 t2) (tp-equal-p t1 t3) (tp-equal-p t2 t4))))

(defun eval-precond (e1 e2 &optional (tg *tg*))
  (let ((t1 (get-beg tg e1))
        (t2 (get-end tg e1))
        (t3 (get-beg tg e2))
        (t4 (get-end tg e2)))
    (and (tp-before-p t1 t2) (tp-before-p t2 t3) (tp-before-p t3 t4))))

(defun eval-during (e1 e2 &optional (tg *tg*))
  (let ((t1 (get-beg tg e1))
        (t2 (get-end tg e1))
        (t3 (get-beg tg e2))
        (t4 (get-end tg e2)))
    (and (tp-before-p t1 t2) (tp-before-p t3 t4) (tp-before-p t3 t1) (tp-before-p t2 t4))))

; Quantitative Bounding Functions
; -------------------------------------------------------------------------

(defun update-lower-bound (e1 ts &optional (tg *tg*))
  (let* ((tp (get-beg tg e1))
         (upper-ts (tp-upper tp)))
    ;; First check if the timebound is inconsistent. Since optimal bounds are
    ;; always propagated, this amounts to just checking the upper bound on
    ;; this point.
    (when (and upper-ts (local-time:timestamp< upper-ts ts))
      (format t "Error: inconsistent lower bound ~A on episode ~A" ts e1)
      (return-from update-lower-bound))
    ;; Now, we are free to add the bound and propagate.
    (update-lower tp ts)))

(defun update-upper-bound (e1 ts &optional (tg *tg*))
  (let* ((tp (get-end tg e1))
         (lower-ts (tp-lower tp)))
    ;; First check if the timebound is inconsistent. Since optimal bounds are
    ;; always propagated, this amounts to just checking the upper bound on
    ;; this point.
    (when (and lower-ts (local-time:timestamp> lower-ts ts))
      (format t "Error: inconsistent upper bound ~A on episode ~A" ts e1)
      (return-from update-upper-bound))
    ;; Now, we are free to add the bound and propagate.
    (update-upper tp ts)))

; Printing Functions
; -------------------------------------------------------------------------

;; prints tg to a graphviz format.
(defun print-tg (tg) 
  (let ((seen (make-hash-table :test #'equal))
        (timepoints nil))

    (format t "digraph T {~%node [shape=record]~%")

    (maphash (lambda (key value) 
               (declare (ignore key))
               (when (not (gethash value seen))
                 (setf (gethash value seen) t)
                 (push value timepoints)))
             (tg-beg tg))
    (maphash (lambda (key value) 
               (declare (ignore key))
               (when (not (gethash value seen))
                 (setf (gethash value seen) t)
                 (push value timepoints)))
             (tg-end tg))

    (dolist (tp timepoints)
      (format t "~A [label=\"hash: ~A\\lptime: ~A\\lbegins: ~A\\lends: ~A\\l\"]~%" 
              (sxhash tp) (sxhash tp) (tp-ptime tp) (tp-brefs tp) (tp-erefs tp)))

    (dolist (tp timepoints)
      (when (tp-next tp)
        (format t "~A -> ~A [color=\"blue\"]~%" (sxhash tp) (sxhash (link-dst (tp-next tp)))))
      (when (tp-out tp)
        (dolist (out (tp-out tp))
          (format t "~A -> ~A~%" (sxhash tp) (sxhash (link-dst out))))))
    (format t "}")))
