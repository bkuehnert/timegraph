(load "macro.lisp")

;;; * chain: Unique chain identifier. Two timepoints will have the same
;;;   chain value iff they are on the same chain.
;;;
;;; * prev: Previous timepoint in the chain.
;;;
;;; * next: Next timepoint in the chain.
;;;
;;; * ptime: Pseudotime of a timepoint
;;;
;;; * inc: List of timepoints that have cross chain links into the
;;;   timepoint.
;;;
;;; * out: List of timepoints that have cross chain links out of the
;;;   timepoint.
;;;
;;; * upper: Numerical absolute upper bound on the timepoint.
;;;
;;; * lower: Numerical absolute lower bound on the timepoint.
;;;
;;; * refs: List of episode names which point to the timepoint.

(defclass timepoint ()
  ((chain :initarg :chain
		  :accessor tp-chain)
   (prev :initarg :prev
		 :accessor tp-prev)
   (next :initarg :next
		 :accessor tp-next)
   (ptime :initarg :ptime
		  :accessor tp-ptime)
   (inc :initarg :inc
		:accessor tp-inc)
   (out :initarg :out
		:accessor tp-out)
   (upper :initarg :upper
		  :accessor tp-upper)
   (lower :initarg :lower
		  :accessor tp-lower)
   (brefs :initarg :brefs
		  :accessor tp-brefs)
   (erefs :initarg :erefs
		  :accessor tp-erefs)))

(defun make-timepoint (&key (chain (sxhash (gensym))) 
							prev next (ptime 1) in out upper lower brefs erefs)
  (make-instance 'timepoint
				 :chain chain
				 :prev prev
				 :next next
				 :ptime ptime
				 :inc in
				 :out out
				 :upper upper
				 :lower lower
				 :brefs brefs
				 :erefs erefs))

;;; Utility Functions
;;; -----------------------------------------------------------------------

;;; Get list of direct successors of a timepoint t1
(defun get-successors (t1)
  (cond ((not t1) nil)
		((not (tp-next t1)) (tp-out t1))
		(t (cons (tp-next t1) (tp-out t1)))))

;;; Get list of direct ancestors of a timepoint t1
(defun get-ancestors (t1)
  (cond ((not t1) nil)
		((not (tp-prev t1)) (tp-inc t1))
		(t (cons (tp-prev t1) (tp-inc t1)))))

;;; Get list of all successors of a timepoint t1
(defun get-all-successors (t1)
  (flatten
	(funcall 
	  (alambda (t1 seen)
	    (cond ((not (gethash t1 seen))
			   (setf (gethash t1 seen) t)
			   (cons t1 (mapcar (lambda (tk) 
								  (self tk seen)) (get-successors t1))))))
		t1 (make-hash-table :test #'equal))))

;;; Get list of all ancestors of a timepoint t1
(defun get-all-ancestors (t1)
  (flatten
	(funcall 
	  (alambda (t1 seen)
	    (cond 
		  ((not (gethash t1 seen))
	       (setf (gethash t1 seen) t)
	       (cons t1 (mapcar (lambda (tk) 
							  (self tk seen)) (get-ancestors t1))))))
		t1 (make-hash-table :test #'equal))))

;;; Check if a timepoint t1 is the last in its chain
(defun last-p (t1)
  (not (tp-next t1)))

;;; Check if a timepoint t1 is the first in its chain
(defun first-p (t1)
  (not (tp-prev t1)))

;;; Insertion methods
;;; -----------------------------------------------------------------------

;;; Given a timepoint t1, creates and returns a new timepoint which is
;;; directly after t1 in the graph.
(defun insert-timepoint-after (t1 &key brefs erefs)
  (let ((ret (gensym)))
	(cond 
	  ((not t1) nil) ;;; add error message here
	  ((last-p t1)
	   (setf ret (make-timepoint
				   :chain (tp-chain t1)
				   :prev t1
				   :ptime (1+ (tp-ptime t1))
				   :lower (tp-lower t1)
				   :brefs brefs
				   :erefs erefs))
	   (setf (tp-next t1) ret)
	   ret)
	  (t
		(setf ret (make-timepoint 
					:in (list t1) 
					:lower (tp-lower t1)
					:brefs brefs
					:erefs erefs))
		(setf (tp-out t1) (cons ret (tp-out t1)))
		ret))))

;;; Given a timepoint t1, creates and returns a new timepoint which is
;;; directly before t1 in the graph.
(defun insert-timepoint-before (t1 &key brefs erefs)
  (let ((ret (gensym)))
	(cond 
	  ((first-p t1)
	   (setf ret (make-timepoint
				   :chain (tp-chain t1)
				   :next t1
				   :ptime (1- (tp-ptime t1))
				   :upper (tp-upper t1)
				   :brefs brefs
				   :erefs erefs))
	   (setf (tp-prev t1) ret)
	   ret)
	  (t
		(setf ret (make-timepoint 
					:out (list t1) 
					:upper (tp-upper t1)
					:brefs brefs
					:erefs erefs))
		(setf (tp-inc t1) (cons ret (tp-inc t1)))
		ret))))

;;; t1 and t2 are timepoints (either are possibly nil), assert that t1 is
;;; before t2 and return them. Preconditions: If t1 and t2 are not nil,
;;; then it must be the case that t2 is not before t1, otherwise, the
;;; timegraph will be in a contradictory state after running this
;;; function.
(defun tp-assert-before (t1 t2)
  (cond
	((tp-before-p t1 t2)
	 (list t1 t2))

	((and (not t1) (not t2))
	 (let* ((t1 (make-timepoint))
			(t2 (insert-timepoint-after t1)))
	   (list t1 t2)))

	((not t1)
	 (list (insert-timepoint-before t2) t2))

	((not t2)
	 (list t1 (insert-timepoint-after t1)))

    ((and (last-p t1) (first-p t2))
     (setf (tp-next t1) t2)
     (setf (tp-prev t2) t1)
     (funcall (alambda (tk ptime hash)
		(when tk
		  (setf (tp-ptime tk) (+ ptime (tp-ptime tk)))
		  (setf (tp-chain tk) hash)
		  (self (tp-next tk) ptime hash)))
      t2 (tp-ptime t1) (tp-chain t1)))

    (t
     (push t2 (tp-out t1))
     (push t1 (tp-inc t2)))))

;;; t1 and t2 are timepoints (either are possibly nil), assert that t1 is
;;; equal to t2 and return (t1 t2). Preconditions: none. Function also
;;; requires reference to a timegraph tg, since updates to the timegraph's
;;; references are needed in some cases.
(defun tp-assert-equals (tg t1 t2)
  (cond 
	((tp-equals-p t1 t2)
	 (list t1 t2))

	((and (not t1) (not t2))
	 (let ((t1 (make-timepoint)))
	   (list t1 t1)))

	((not t1)
	 (list t2 t2))

	((not t2)
	 (list t1 t1))

	((tp-before-p t1 t2)
	 (tp-assert-equal-helper tg t1 t2)
	 (list t1 t2))

	((tp-before-p t2 t1)
	 (tp-assert-equal-helper tg t2 t1)
	 (list t1 t2))))

;;; In the case that t1 and t2 exist and t2 is after t1, then in order
;;; to assert tat t1 = t2, all timepoints between t2 and t1 must be
;;; set equal to eachother (and thus equal to t1). This helper function
;;; searches for such points and updates their references with a given
;;; timegraph object tg.
(defun tp-assert-equal-helper (tg t1 t2)
  (let* ((t1suc (get-all-successors t1))
		 (t2anc (get-all-ancestors t2))
		 (quo (intersection t1suc t2anc)))
	(dolist (tk quo)
	  (when (tp-prev tk)
		(setf (tp-inc t1) (adjoin (tp-prev tk) (tp-inc t1)))
		(setf (tp-out (tp-prev tk)) (adjoin tk (tp-out (tp-prev tk))))
		(setf (tp-next (tp-prev tk)) nil))
	  (when (tp-next tk)
		(setf (tp-inc t1) (adjoin (tp-prev tk) (tp-inc t1)))
		(setf (tp-out (tp-prev tk)) (adjoin tk (tp-out (tp-prev tk))))
		(setf (tp-prev (tp-next tk)) nil))

	  (setf (tp-inc t1) (union (tp-inc t1)
				   (remove-if (lambda (x) (member x quo))
						    (tp-inc tk))))
	  (setf (tp-out t1) (union (tp-out t1)
				   (remove-if (lambda (x) (member x quo))
						    (tp-out tk))))

	  (setf (tp-brefs t1) (union (tp-brefs t1) (tp-brefs tk)))
	  (setf (tp-erefs t1) (union (tp-erefs t1) (tp-erefs tk)))
	  (dolist (bref (tp-brefs tk))
		(setf (gethash bref (first tg)) t1))
	  (dolist (eref (tp-erefs tk))
		(setf (gethash eref (second tg)) t1)))))

;;; Querying functions
;;; ----------------------------------------------------------------------

;;; Returns t if t1 is before or equal to t2. Returns nil if t1 is after
;;; t2 or there is no relation found.
(defun tp-before-p (t1 t2)
  (if (or (not t1) (not t2))
	nil
	(funcall 
	  (alambda (src dst seen) 
	    (cond
		  ((and (equal (tp-chain src) (tp-chain dst)))
		   (<= (tp-ptime src) (tp-ptime dst)))
		  ((not (gethash src seen))
		   (setf (gethash src seen) t)
		   (dolist (node (get-successors src))
			 (if (self node dst seen)
			   t)))))
	  t1 t2 (make-hash-table :test #'equal))))

;;; Returns t if and only if the timegraph contains evidence that t1 is 
;;; not before t2.
(defun tp-not-before-p (t1 t2) 
  (and (not (equal t1 t2))
	   (tp-before-p t2 t1)))

;;; Returns t if t1 is equal to t2. Returns nil if the inference cannot be
;;; made.
(defun tp-equals-p (t1 t2)
  (if (or (not t1) (not t2))
	nil
  	(equal t1 t2)))

;;; Returns t if the inference that t1 is not equal to t2 can be made. 
;;; Note: due to the strength of timegraph, this inference can never be
;;; made.
(defun tp-not-equals-p (t1 t2)
  nil)

;;; For two timepoints t1 and t2, compute the relation (if one exists)
;;; between the two timepoints. Possible return values are:
;;; 	- nil : no relation found
;;; 	- 1   : t1 before or equals t2
;;;     - 2   : t1 after or equals t2 
;;;     - 3   : t1 equal to t2

(defun get-relation (t1 t2)
  (cond
	((or (not t1) (not t2)) nil)
	((and (equal (tp-chain t1) (tp-chain t2)) 
		  (< (tp-ptime t1) (tp-ptime t2))) 1)
	((and (equal (tp-chain t1) (tp-chain t2)) 
		  (> (tp-ptime t1) (tp-ptime t2))) 2)
	((equal t1 t2) 3)
	((tp-before-p t1 t2) 1)
	((tp-before-p t2 t1) 2)
	(t nil)))


;;; Quantitative bounds (needs some work)
;;; ----------------------------------------------------------------------

;(defun insert-lower-bound (t1 bound)
;  (progn
;	(setf (tp-lower t1) bound)
;	(prop-lower-bound t1)))
;
;(defun insert-upper-bound (t1 bound)
;  (progn
;	(setf (tp-upper t1) bound)
;	(prop-upper-bound t1)))
;
;;;; Propogate lower bound
;(defun prop-lower-bound (t1)
;  (dolist (tk (get-successors t1))
;	(prop-bound-down tk  (tp-lower t1))))
;
;;;; Propogate upper bound
;(defun prop-upper-bound (t1)
;  (dolist (tk (get-ancestors t1))
;	(prop-bound-up tk (tp-upper t1))))
;
;(defun prop-bounds (t1)
;  (progn
;	(prop-upper-bound t1)
;	(prop-lower-bound t1)))
;
;;; Propogate timebounds up
;(defun prop-bound-down (t1 bound)
;  (cond 
;	((not t1) nil)
;	((or (not (tp-lower t1)) (< (tp-lower t1) bound))
;	 (setf (tp-lower t1) bound)
;	 (dolist (tk (get-successors t1)) 
;	   (prop-bound-down tk bound)))))
;
;;;; Propogate timebounds down
;(defun prop-bound-up (t1 bound)
;  (cond
;	((not t1) nil)
;	((or (not (tp-upper t1)) (> (tp-upper t1) bound))
;	 (setf (tp-upper t1) bound)
;	 (dolist (tk (get-ancestors t1)) 
;	   (prop-bound-up tk bound)))))

;;; testing functions
;;; -----------------------------------------------------------------------

(defun print-tp (tp)
  (format t "prev: ~A~%next: ~A~%links: ~A"
  (tp-prev tp) (tp-next tp) (tp-out tp)))
