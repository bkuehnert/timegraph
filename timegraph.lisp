(in-package #:timegraph.implementation)
;;; A timegraph is a pair of hashtable. The first hashtable resolves an
;;; episode symbol to the timepoint representing the start of the
;;; episode. Similarly, the second hashtable resolves an episode symbol
;;; to the timepoint representing the end of the episode.
(defstruct (timegraph (:conc-name tg-))
           (beg (make-hash-table :test #'equal) :type hash-table)
           (end (make-hash-table :test #'equal) :type hash-table))

;; Global timegraph to be used.
(defvar *tg* (make-timegraph))


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

;; Pre-defined functions to express episodic relationships on timepoints

(defun tg-assert-before (e1 e2 &optional (tg *tg*))
  (let* ((pair1 (tp-assert-before tg (get-beg tg e1) (get-end tg e1)))
         (pair2 (tp-assert-before tg (first pair1) (get-beg tg e2)))
         (pair3 (tp-assert-before tg (second pair2) (get-end tg e2))))
    (set-beg tg e1 (first pair2))
    (set-end tg e1 (second pair1))
    (set-beg tg e2 (first pair3))
    (set-end tg e2 (second pair3))))

(defun tg-assert-consec (e1 e2 &optional (tg *tg*))
  (let* ((pair1 (tp-assert-before tg (get-beg tg e1) (get-end tg e1)))
         (pair2 (tp-assert-equal tg (second pair1) (get-beg tg e2)))
         (pair3 (tp-assert-before tg (second pair2) (get-end tg e2))))
    (set-beg tg e1 (first pair1))
    (set-end tg e1 (first pair2))
    (set-beg tg e2 (first pair3))
    (set-end tg e2 (second pair3))))

(defun tg-assert-equal (e1 e2 &optional (tg *tg*))
  (let* ((pair1 (tp-assert-before tg (get-beg tg e1) (get-end tg e1)))
         (pair2 (tp-assert-equal tg (first pair1) (get-beg tg e2)))
         (pair3 (tp-assert-equal tg (second pair1) (get-end tg e2))))
    (set-beg tg e1 (first pair2))
    (set-end tg e1 (first pair3))
    (set-beg tg e2 (second pair2))
    (set-end tg e2 (second pair3))))

#|
;; fake version
(defun tg-assert-equal (e1 e2 &optional (tg *tg*))
  (let* ((pair1 (tp-assert-before tg (get-beg tg e1) (get-end tg e1)))
         (pair2 (tp-assert-before tg (get-beg tg e2) (get-end tg e2)))
         (pair3 (tp-assert-equal tg (first pair1) (first pair2))))
    (set-beg tg e1 (first pair3))
    (set-end tg e1 (second pair1))
    (set-beg tg e2 (second pair3))
    (set-end tg e2 (second pair2))))
|#

(defun tg-assert-precond (e1 e2 &optional (tg *tg*))
  (let* ((pair1 (tp-assert-before tg (get-beg tg e1) (get-end tg e1)))
         (pair2 (tp-assert-before tg (first pair1) (get-beg tg e2)))
         (pair3 (tp-assert-equal tg (second pair1) (get-end tg e2))))
    (set-beg tg e1 (first pair2))
    (set-end tg e1 (first pair3))
    (set-beg tg e2 (second pair2))
    (set-end tg e2 (second pair3))))

(defun tg-assert-at-about (e1 e2 &optional (tg *tg*))
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

(defun eval-at-about (e1 e2 &optional (tg *tg*))
  (let ((t1 (get-beg tg e1))
        (t2 (get-end tg e1))
        (t3 (get-beg tg e2))
        (t4 (get-end tg e2)))
    (and (tp-before-p t1 t2) (tp-before-p t3 t4) (tp-before-p t3 t1) (tp-before-p t2 t4))))

;; A timegraph proposition is in the form (E1 REL E2) where E1 and E2 are symbols
;; representing episodes, and REL is a symbol representing a relation, for example,
;; EQUAL. This macro verifies that a proposition is valid.
(defun time-prop-p (prop)
  (and 
   (listp prop)
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
      (:at-about (tg-assert-at-about e1 e2 tg)))))

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
      (:at-about (eval-at-about e1 e2 tg)))))

;; The above code can be automatically generated, and it should be done that way to make
;; extensibility less error-prone. However, I tried multiple times to write the code that does
;; this, and each time it was unsuccessful. Below is a version of that code that almost works.
#|
;; Translates relation
(defvar *tg-relations* (make-hash-table :test #'equal))

;; Defines an binary episodic relation, and adds the necessary functions to *tg-relations*
(defun create-ep-relation (name conditions)
  (setf (gethash name *tg-relations*)
        (list (eval (create-rel-assert conditions))
              (eval (create-rel-eval conditions))
              (eval (create-rel-eval-neg conditions)))))

(defun create-rel-assert-help (con tg e1 e2)
  `(let* ((e1.start (get-beg ,tg ,e1))
          (e2.start (get-end ,tg ,e1))
          (e1.end (get-beg ,tg ,e2))
          (e2.end (get-end ,tg ,e2))
          (pair (,(if (equal (first con) '=) 'tp-assert-equal 'tp-assert-before)
                  ,tg ,(second con) ,(third con)))
          (,(second con) (first pair))
          (,(third con) (second pair)))
     (when e1.start
       (set-beg ,tg ,e1 e1.start)
       (pushnew ,e1 (tp-brefs e1.start)))
     (when e1.end
       (set-end ,tg ,e1 e1.end)
       (pushnew ,e1 (tp-erefs e1.end)))
     (when e2.start
       (set-beg ,tg ,e2 e2.start)
       (pushnew ,e2 (tp-brefs e2.start)))
     (when e2.end
       (set-end ,tg ,e2 e2.end)
       (pushnew ,e2 (tp-erefs e2.end)))))

(defun create-rel-assert (conditions)
  (let ((e1 (gensym "E1"))
        (e2 (gensym "E2"))
        (tg (gensym "TG")))
    `(lambda (,tg ,e1 ,e2)
       ,@(mapcar (lambda (con) (create-rel-assert-help con tg e1 e2))
                 conditions))))

(defun create-rel-eval (conditions)
  (let ((e1 (gensym "E1"))
        (e2 (gensym "E2"))
        (tg (gensym "TG")))
    `(lambda (,tg ,e1 ,e2)
       (let* ((e1.start (get-beg ,tg ,e1))
              (e2.start (get-end ,tg ,e2))
              (e1.end (get-beg ,tg ,e1))
              (e2.end (get-end ,tg ,e2)))
         (and ,@(mapcar (lambda (con)
                          `(,(if (equal (first con) '=)
                                 'tp-equal-p
                                 'tp-before-p)
                             ,(second con)
                             ,(third con)))
                        conditions))))))

(defun create-rel-eval-neg (conditions)
  (let ((e1 (gensym "E1"))
        (e2 (gensym "E2"))
        (tg (gensym "TG")))
    `(lambda (,tg ,e1 ,e2)
       (let* ((e1.start (get-beg ,tg ,e1))
              (e2.start (get-end ,tg ,e2))
              (e1.end (get-beg ,tg ,e1))
              (e2.end (get-end ,tg ,e2)))
         (or ,@(mapcar (lambda (con)
                         `(,(if (equal (first con) '=)
                                'tp-not-equal-p
                                'tp-not-before-p)
                            ,(second con)
                            ,(third con)))
                       conditions))))))

;; Add some typical relations
(create-ep-relation 'equal '((< e1.start e1.end) (= e1.start e2.start) (= e1.end e2.end)))
(create-ep-relation 'before '((< e1.start e2.start) (< e1.start e1.end) (< e2.start e2.end)))
(create-ep-relation 'after '((< e1.start e1.end) (< e2.start e2.end) (< e2.start e1.start)))
(create-ep-relation 'consec '((< e1.start e1.end) (= e1.end e2.start) (< e2.start e2.end)))
(create-ep-relation 'at-about '((< e1.start e1.end) (< e2.start e2.end) (< e2.start e1.start) (< e1.end e2.end)))
(create-ep-relation 'precond-of '((< e1.start e1.end) (< e1.end e2.start) (< e2.start e2.end)))
(create-ep-relation 'postcond-of '((< e1.start e1.end) (< e2.end e1.start) (< e2.start e2.end)))
|#



; Quantitative Bounding functions
; -------------------------------------------------------------------------

;(defun set-lower-timebound (tg tp bound)
;  (setf (gethash tp (tg-lower tg)) bound))
;
;(defun set-upper-timebound (tg tp bound)
;  (setf (gethash tp (tg-upper tg)) bound))
;
;(defun set-lower-duration (tg t1 t2 bound) 
;  (setf (gethash (list t1 t2) (tg-lower tg)) bound))
;
;(defun set-upper-duration (tg t1 t2 bound) 
;  (setf (gethash (list t1 t2) (tg-upper tg)) bound))
;
;(defun get-lower-timebound (tg tp)
;  (gethash tp (tg-lower tg)))
;
;(defun get-upper-timebound (tg tp)
;  (gethash tp (tg-upper tg)))
;
;(defun get-lower-duration (tg t1 t2) 
;  (gethash (list t1 t2) (tg-lower tg)))
;
;(defun get-upper-duration (tg t1 t2) 
;  (gethash (list t1 t2) (tg-upper tg)))
;
;(defun insert-lower-bound (tg tp bound)
;  (let ((l1 (get-lower-timebound tg tp)))
;       (when (or (not l1) (< l1 bound))
;         (set-lower-timebound tg tp bound)
;         (dolist (tk (get-successors tp))
;               (let ((l2 (get-lower-timebound tg tk))
;                         (l (get-lower-duration tg tp tk)))
;                 (if (or (not l2) (< l2 (+ bound (fixnil l))))
;                       (insert-lower-bound tg tk (+ bound (fixnil l))))
;                 (refresh-optimal-duration tg tp tk))))))
;
;(defun insert-upper-bound (tg tp bound)
;  (let ((u2 (get-upper-timebound tg tp)))
;       (when (or (not u2) (> u2 bound))
;         (set-upper-timebound tg tp bound)
;         (dolist (tk (get-ancestors tp))
;               (let* ((u1 (get-upper-timebound tg tk))
;                          (u (get-upper-duration tg tp tk)))
;                 (if (or (not u1) (> u1 (- bound (fixnil u))))
;                       (insert-upper-bound tg tk (- bound (fixnil u))))
;                 (refresh-optimal-duration tg tk tp))))))
;
;(defun insert-lower-duration (tg t1 t2 bound)
;  (let ((l1 (get-lower-timebound tg t1))
;               (u1 (get-upper-timebound tg t1))
;               (l2 (get-lower-timebound tg t2))
;               (u2 (get-upper-timebound tg t2))
;               (l (get-lower-duration tg t1 t2)))
;
;       (when (or (not l) (< l bound))
;         (if (and (not l1) (or (not l2) (< l2 (+ bound l1)))) 
;               (insert-lower-bound tg t2 (+ bound l1)))
;         (if (and (not u2) (or (not u1) (> u1 (- u2 bound))))
;               (insert-upper-bound tg t1 (- u2 bound))))))
;
;(defun insert-upper-duration (tg t1 t2 bound)
;  (let ((l1 (get-lower-timebound tg t1))
;               (u1 (get-upper-timebound tg t1))
;               (l2 (get-lower-timebound tg t2))
;               (u2 (get-upper-timebound tg t2))
;               (u (get-upper-duration tg t1 t2)))
;
;       (when (or (not u) (> u bound))
;         (if (and (not u1) (or (not u2) (> u2 (+ bound u1)))) 
;               (insert-upper-bound tg t2 (+ bound u1)))
;         (if (and (not l2) (or (not l1) (< l1 (- l2 bound))))
;               (insert-lower-bound tg t1 (- l2 bound))))))
;  
;
;(defun refresh-optimal-duration (tg t1 t2)
;  (let ((l1 (get-lower-timebound tg t1))
;               (u1 (get-upper-timebound tg t1))
;               (l2 (get-lower-timebound tg t2))
;               (u2 (get-upper-timebound tg t2))
;               (l (get-lower-duration tg t1 t2))
;               (u (get-upper-duration tg t1 t2)))
;
;       (if (or (not l) (and l2 u1 (< l (- l2 u1))))
;         (set-lower-duration tg t1 t2 (- l2 u1)))
;       (if (or (not u) (and u2 l1 (> u (- u2 l1))))
;         (set-upper-duration tg t1 t2 (- u2 l1)))))

; Printing Functions
; -------------------------------------------------------------------------

;; prints tg to a graphviz format
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
