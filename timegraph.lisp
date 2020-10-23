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

;; Translates relation
(defvar *tg-relations* (make-hash-table :test #'equal))

;; Accessor functions
(defun get-beg (tg e1)
  (gethash e1 (tg-beg tg)))

(defun get-end (tg e1)
  (gethash e1 (tg-end tg)))

(defun set-beg (tg e1 tp)
  (setf (gethash e1 (tg-beg tg)) tp))

(defun set-end (tg e1 tp)
  (setf (gethash e1 (tg-end tg)) tp))

  ;; Defines an binary episodic relation, and adds the necessary functions to *tg-relations*
(defun create-ep-relation (name conditions)
  (setf (gethash name *tg-relations*)
        (list (eval (create-rel-assert conditions))
              (eval (create-rel-eval conditions))
              (eval (create-rel-eval-neg conditions)))))

(defun create-rel-assert (conditions)
  (let ((e1 (gensym "E1"))
        (e2 (gensym "E2"))
        (tg (gensym "TG")))
  `(lambda (,tg ,e1 ,e2)
     (let* ((e1.start (get-beg ,tg ,e1))
            (e2.start (get-end ,tg ,e1))
            (e1.end (get-beg ,tg ,e2))
            (e2.end (get-end ,tg ,e2))
            ,@(apply #'append
                     (mapcar
                      (lambda (con)
                        `(,(cond
                             ((equal (first con) '=)
                              `(pair (tp-assert-equals ,tg ,(second con) ,(third con))))
                             ((equal (first con) '<)
                              `(pair (tp-assert-before ,(second con) ,(third con)))))
                           (,(second con) (first pair))
                           (,(third con) (second pair))))
                      conditions)))
       ;; This is probably unnecessary, since all of the timepoint update functions will
       ;; update the references against the timegraph which is passed in.
       ;; TODO: Experiment with removing this.
       (set-beg ,tg ,e1 e1.start)
       (set-end ,tg ,e1 e1.end)
       (set-beg ,tg ,e2 e2.start)
       (set-end ,tg ,e2 e2.end) 
       (pushnew ,e1 (tp-brefs e1.start))
       (pushnew ,e1 (tp-erefs e1.end))
       (pushnew ,e2 (tp-brefs e2.start))
       (pushnew ,e2 (tp-erefs e2.end))))))

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
                                 'tp-equals-p
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
                                'tp-not-equals-p
                                'tp-not-before-p)
                            ,(second con)
                            ,(third con)))
                       conditions))))))

;; Add some typical relations
(create-ep-relation 'equals '((< e1.start e1.end) (= e1.start e2.start) (= e1.end e2.end)))
(create-ep-relation 'before '((< e1.start e2.start) (< e1.start e1.end) (< e2.start e2.end)))
(create-ep-relation 'after '((< e1.start e1.end) (< e2.start e2.end) (< e2.start e1.start)))
(create-ep-relation 'consec '((< e1.start e1.end) (= e1.end e2.start) (< e2.start e2.end)))
(create-ep-relation 'at-about '((< e1.start e1.end) (< e2.start e2.end) (< e2.start e1.start) (< e1.end e2.end)))
(create-ep-relation 'precond-of '((< e1.start e1.end) (< e1.end e2.start) (< e2.start e2.end)))
(create-ep-relation 'postcond-of '((< e1.start e1.end) (< e2.end e1.start) (< e2.start e2.end)))

;; A timegraph proposition is in the form (E1 REL E2) where E1 and E2 are symbols
;; representing episodes, and REL is a symbol representing a relation, for example,
;; EQUALS. This macro verifies that a proposition is valid.
(defun time-prop-p (prop)
  (and 
   (listp prop)
   (equal (list-length prop) 3)
   (gethash (second prop) *tg-relations*)
   t)) ; t here so that we return a boolean.

;; Asserts the proposition PROP holds in timegraph TG. By default, the global timegraph
;; is used.
(defun assert-prop (prop &key (tg *tg*))
  (funcall (first (gethash (second prop) *tg-relations*))
           tg (first prop) (third prop)))

;; Evaluates the proposition PROP in timegraph TG. Returns 1 if true, 0 if false,
;; and 2 if indeterminate. By default, teh global timegraph is used.
(defun eval-prop (prop &key (tg *tg*))
  (let ((tuple (gethash (second prop) *tg-relations*))
        (e1 (first prop))
        (e2 (third prop)))
    (cond
      ((funcall (second tuple) tg e1 e2) 1)
      ((funcall (third tuple) tg e1 e2) 0)
      (t 2))))

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
               (when (not (gethash value seen))
                 (setf (gethash value seen) t)
                 (push value timepoints)))
             (tg-beg tg))
    (maphash (lambda (key value) 
               (when (not (gethash value seen))
                 (setf (gethash value seen) t)
                 (push value timepoints)))
             (tg-end tg))

    (dolist (tp timepoints)
      (format t "~A [label=\"ptime: ~A\\lbegins: ~A\\lends: ~A\\l\"]~%" 
              (sxhash tp) (tp-ptime tp) (tp-brefs tp) (tp-erefs tp)))

    (dolist (tp timepoints)
      (when (tp-next tp)
        (format t "~A -> ~A [color=\"blue\"]~%" (sxhash tp) (sxhash (link-dst (tp-next tp)))))
      (when (tp-out tp)
        (dolist (out (tp-out tp))
          (format t "~A -> ~A~%" (sxhash tp) (sxhash (link-dst out))))))
    (format t "}")))
