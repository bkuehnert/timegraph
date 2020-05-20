(load "macro.lisp")

;;; * chain: Unique chain identifier. Two timepoints will have the same
;;;   chain value iff they are on the same chain.
;;;
;;; * prev: Link object corresponding to the previous link in the chain.
;;;
;;; * next: Link object corresponding to the next link in the chain.
;;;
;;; * ptime: Pseudotime of a timepoint.
;;;
;;; * inc: List of cross-chain links entering the timepoint.
;;;
;;; * out: List of cross-chain links exiting the timepoint.
;;;
;;; * upper: Numerical absolute upper bound on the timepoint.
;;;
;;; * lower: Numerical absolute lower bound on the timepoint.
;;;
;;; * brefs: List of episode names which the timepoint begins.
;;; 
;;; * erefs: List of episode names which the timepoint ends.

(defclass timepoint ()
  ((chain :initarg :chain
	  :accessor tp-chain)

   (prev :initarg :prev
	 :accessor tp-prev)

   (next :initarg :next
	 :accessor tp-next)

   (ptime :initarg :ptime
	  :accessor tp-ptime)

   (upper :initarg :upper
	  :accessor tp-upper)

   (lower :initarg :lower
	  :accessor tp-lower)

   (inc :initarg :inc
	:accessor tp-inc)

   (out :initarg :out
	:accessor tp-out)

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


(defclass link ()
  ((chain :initarg :chain
	  :accessor link-chain)
   (src :initarc :src
	:accessor link-src)
   (dst :initarg :dst
	:accessor link-dst)
   (lower :initarg :lower
	  :accessor link-lower)
   (upper :initarg :upper
	  :accessor link-upper)))

(defun make-link (&key chain src dst lower upper)
  (make-instance 'link
		 :chain chain
		 :src src
		 :dst dst
		 :lower lower
		 :upper upper))

;;; Utility Functions
;;; -----------------------------------------------------------------------

;;; Get list of direct successors of a timepoint t1
(defun get-successors (t1)
  (when t1
    (let ((succ (mapcar (lambda (x) (link-dst x)) (tp-out t1))))
      (if (tp-next t1)
	  (cons (link-dst (tp-next t1)) succ)
	  succ))))

;;; Get list of direct ancestors of a timepoint t1
(defun get-ancestors (t1)
  (when t1
    (let ((anc (mapcar (lambda (x) (link-src x)) (tp-inc t1))))
      (if (tp-prev t1)
	  (cons (link-src (tp-prev t1)) anc)
	  anc))))

;;; Get list of all successors of a timepoint t1
(defun get-all-successors (t1)
  (flatten
   (funcall 
    (alambda (t1 seen)
	     (cond ((not (gethash t1 seen))
		    (setf (gethash t1 seen) t)
		    (cons t1 (mapcar (lambda (tk) 
				       (self tk seen))
				     (get-successors t1))))))
    t1 (make-hash-table :test #'equal))))

;;; Get list of all ancestors of a timepoint t1.
(defun get-all-ancestors (t1)
  (flatten
   (funcall 
    (alambda (t1 seen)
	     (cond 
	       ((not (gethash t1 seen))
		(setf (gethash t1 seen) t)
		(cons t1 (mapcar (lambda (tk) 
				   (self tk seen))
				 (get-ancestors t1))))))
    t1 (make-hash-table :test #'equal))))

;;; Check if a timepoint t1 is the last in its chain.
(defun last-p (t1)
  (not (tp-next t1)))

;;; Check if a timepoint t1 is the first in its chain.
(defun first-p (t1)
  (not (tp-prev t1)))

;;; For two existing timepoints t1 and t2, establish a cross-chain link t1 -> t2.
(defun add-cross-link (t1 t2 )
  (when (and t1 t2)
    (let ((newlink (make-link :src t1 :dst t2)))
      (push newlink (tp-out t1))
      (push newlink (tp-inc t2)))))

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
		  :ptime (1+ (tp-ptime t1))
		  :lower (tp-lower t1)
		  :brefs brefs
		  :erefs erefs))
       (let ((newlink (make-link :chain (tp-chain t1) :src t1 :dst ret)))
	 (setf (tp-prev ret) newlink)
	 (setf (tp-next t1) newlink))

       ret)

      (t
       (setf ret (make-timepoint :brefs brefs :erefs erefs))
       (add-cross-link t1 ret)
       ret))))

;;; Given a timepoint t1, creates and returns a new timepoint which is
;;; directly before t1 in the graph.
(defun insert-timepoint-before (t1 &key brefs erefs)
  (let ((ret (gensym)))
    (cond 
      ((not t1) nil) ; add error

      ((first-p t1)
       (setf ret (make-timepoint
		  :chain (tp-chain t1)
		  :ptime (1- (tp-ptime t1))
		  :upper (tp-upper t1)
		  :brefs brefs
		  :erefs erefs))
       (let ((newlink (make-link :chain (tp-chain t1) :src ret :dst t1)))
	 (setf (tp-next ret) newlink)
	 (setf (tp-prev t1) newlink))

       ret)
	 
      (t
       (setf ret (make-timepoint :brefs brefs :erefs erefs))
       (add-cross-link ret t1)
       ret))))

;;; t1 and t2 are timepoints (either are possibly nil), assert that t1 is
;;; before t2 and return them. Preconditions: If t1 and t2 are not nil,
;;; then it must be the case that t2 is not before t1, otherwise, the
;;; timegraph will be in a contradictory state after running this
;;; function.
(defun tp-assert-before (t1 t2)
  (cond
    ((and (not t1) (not t2))
     (let* ((t1 (make-timepoint))
	    (t2 (insert-timepoint-after t1)))
       (list t1 t2)))

    ((not t1)
     (list (insert-timepoint-before t2) t2))

    ((not t2)
     (list t1 (insert-timepoint-after t1)))

    ((tp-before-p t1 t2)
     (list t1 t2))

    ((and (last-p t1) (first-p t2))
     (let ((newlink (make-link :chain (tp-chain t1) :src t1 :dst t2)))
       (setf (tp-next t1) newlink)
       (setf (tp-prev t2) newlink))
     (funcall
      (alambda (tk ptime hash)
	(when tk
	  (setf (tp-ptime tk) (+ ptime (tp-ptime tk)))
	  (setf (tp-chain tk) hash)
	  (self (tp-next tk) ptime hash)))
      t2 (tp-ptime t1) (tp-chain t1))
     (list t1 t2))

    (t
     (add-cross-link t1 t2)
     (list t1 t2))))

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
	  (list t1 t1))

	((tp-before-p t2 t1)
	 (tp-assert-equal-helper tg t2 t1)
	 (list t2 t2))

	(t
	  (tp-assert-before t1 t2)
	  (tp-assert-equal-helper tg t1 t2)
	  (list t1 t1))))

;;; In the case that t1 and t2 exist and t2 is after t1, then in order
;;; to assert tat t1 = t2, all timepoints between t2 and t1 must be
;;; set equal to eachother (and thus equal to t1). This helper function
;;; searches for such points and updates their references with a given
;;; timegraph object tg.
(defun tp-assert-equal-helper (tg t1 t2)
  (let* ((t1suc (get-all-successors t1))
	 (t2anc (get-all-ancestors t2))
	 (quo (remove t1 (intersection t1suc t2anc))))

    (when (member (tp-next t1) quo) 
      (setf (tp-next t1) nil))

    (setf (tp-inc t1) (remove-if (lambda (x) (member x quo)) (tp-inc t1)))

    (setf (tp-out t1) (remove-if (lambda (x) (member x quo)) (tp-out t1)))

	(dolist (tk quo)
	  (let ((tk-inc (remove-if (lambda (x) (member x (cons t1 quo)))
							   (tp-inc tk)))
			(tk-out (remove-if (lambda (x) (member x (cons t1 quo))) 
							   (tp-out tk))))

		(when (and (tp-prev tk) (not (member (tp-prev tk) (cons t1 quo))))
		  (setf (tp-next (tp-prev tk)) nil)
		  (tp-add-out-edge (tp-prev tk) t1)
		  (tp-add-inc-edge t1 (tp-prev tk)))
		
		(when (and (tp-next tk) (not (member (tp-next tk) (cons t1 quo))))
		  (setf (tp-prev (tp-next tk)) nil)
		  (tp-add-inc-edge (tp-next tk) t1)
		  (tp-add-out-edge t1 (tp-next tk)))

		(dolist (tj tk-inc)
		  (setf (tp-out tj) (remove tk (tp-out tj)))
		  (tp-add-out-edge tj t1)
		  (tp-add-inc-edge t1 tj))

		(dolist (tj tk-out)
		  (setf (tp-inc tj) (remove tk (tp-inc tj)))
		  (tp-add-inc-edge tj t1)
		  (tp-add-out-edge t1 tj)))



	  (setf (tp-brefs t1) (union (tp-brefs t1) (tp-brefs tk)))
	  (setf (tp-erefs t1) (union (tp-erefs t1) (tp-erefs tk)))

	  (dolist (bref (tp-brefs tk))
		(set-str tg bref t1))
	  (dolist (eref (tp-erefs tk))
		(set-end tg eref t1))))

  (setf (tp-inc t1) (union (tp-inc t1) (tp-inc t2)))
  (setf (tp-out t1) (union (tp-out t1) (tp-out t2)))
  (if (tp-next t2)
	(tp-assert-before t1 (tp-next t2))))


(defun tp-add-out-edge (t1 t2)
  (when (and t1 t2 (not (equal (tp-next t1) t2)))
	(pushnew t2 (tp-out t1))))

(defun tp-add-inc-edge (t1 t2)
  (when (and t1 t2 (not (equal (tp-prev t1) t2)))
	(pushnew t2 (tp-inc t1))))


;;; Querying functions
;;; ----------------------------------------------------------------------

;;; Returns t if t1 is before or equal to t2. Returns nil if t1 is after
;;; t2 or there is no relation found.
;;; todo: add shortcut if quant bounds allow it (with optional tg reference)
(defun tp-before-p (t1 t2)
  (if (or (not t1) (not t2))
	nil
	(block loops
	(funcall 
	  (alambda (src dst seen) 
	    (cond
		  ((and (equal (tp-chain src) (tp-chain dst)))
		   (return-from loops (<= (tp-ptime src) (tp-ptime dst))))
		  ((not (gethash src seen))
		   (setf (gethash src seen) t)
		   (dolist (node (get-successors src))
			 (when (self node dst seen)
			   ;(format t "Searching ~A before ~A" node dst)
			   (return-from loops t))))))
	  t1 t2 (make-hash-table :test #'equal)))))

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
