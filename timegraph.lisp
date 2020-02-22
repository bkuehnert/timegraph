(load "macro.lisp")

;;; If true, will not allow inconsistent relations to be added.
(defvar *enforce-correctness* nil) 

;;; Timepoints have two components: They serve as a linked list (for
;;; internal chain relations) and they also have a list of outgoing edges
;;; that link to other chains.
;;; -----------------------------------------------------------------------
;;; * prev: the previous node in the chain
;;;
;;; * next: the next node in the chain
;;;
;;; * ptime: pseudotime 
;;;
;;; * links: a linked list of cross-chain links to other chains.
 
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
			  :accessor tp-lower)))

;;; IN PROGRESS PLEASE FINISH
;;; 1. upbd -> upper, lwbd -> lower
;;; 2. inlinks -> inc, outlinks -> out
;;; 3. chain moved to timepoint, timegraph structure DELETED
;;; 4. merge insert-timepoint and make-timepoint
;;; 5. insert timepoint functions return timepoints. 
;;; 6. equal -> no longer exists

;;; Create and return an empty timegraph.
(defun make-timegraph ()
  (make-instance 'timegraph
				 :hash (make-hash-table :test #'equal)
				 :chains (make-hash-table :test #'equal)))

;;; Create and return a new timepoint
(defun make-timepoint (&key (chain (sxhash (gensym))) 
							prev next (ptime 1) in out upper lower)
  (make-instance 'timepoint
				 :chain chain
				 :prev prev
				 :next next
				 :ptime ptime
				 :inc in
				 :out out
				 :upper upper
				 :lower lower))

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

;;; Check if a timepoint t1 is the last in its chain
(defun last-p (t1)
  (not (tp-next t1)))

;;; Check if a timepoint t1 is the first in its chain
(defun first-p (t1)
  (not (tp-prev t1)))

;;; -----------------------------------------------------------------------

;;; Given a timepoint t1, creates and returns a new timepoint which is
;;; directly after t1 in the graph.
(defun insert-timepoint-after (t1)
  (let ((ret (gensym)))
	(cond 
	  ((not t1) nil) ;;; add error message here
	  ((last-p t1)
	   (setf ret (make-timepoint
				   :chain (tp-chain t1)
				   :prev t1
				   :ptime (1+ (tp-ptime t1))
				   :lower (tp-lower t1)))
	   (setf (tp-next t1) ret)
	   ret)
	  (t
		(setf ret (make-timepoint :in (list t1) :lower (tp-lower t1)))
		(setf (tp-out t1) (cons ret (tp-out t1)))
		ret))))

;;; Given a timepoint t1, creates and returns a new timepoint which is
;;; directly before t1 in the graph.
(defun insert-timepoint-before (t1)
  (let ((ret (gensym)))
	(cond 
	  ((first-p t1)
	   (setf ret (make-timepoint
				   :chain (tp-chain t1)
				   :next t1
				   :ptime (1- (tp-ptime t1))
				   :upper (tp-upper t1)))
	   (setf (tp-prev t1) ret)
	   ret)
	  (t
		(setf ret (make-timepoint :out (list t1) :upper (tp-upper t1)))
		(setf (tp-inc t1) (cons ret (tp-inc t1)))
		ret))))

;;; Given timepoints t1 and t2, creates and returns a new timepoint
;;; which is after t1 and before t2.
(defun insert-timepoint-during (t1 t2)
  (let ((ret (gensym)))
	(cond
	  ((and (equal (tp-chain t1) (tp-chain t2))
			(equal (tp-next t1) t2))
	   (setf ret (make-timepoint
				   :chain (tp-chain t1)
				   :prev t1
				   :next t2
				   :ptime (/ (+ (tp-ptime t1) (tp-ptime t2)) 2)
				   :lower (tp-lower t1)
				   :upper (tp-upper t2)))
	   (setf (tp-next t1) ret)
	   (setf (tp-prev t2) ret)
	   ret)
	  ((last-p t1)
	   (setf ret (insert-timepoint-after t1))
	   (setf (tp-out ret) (list t2))
	   (push ret (tp-inc t2))
	   (setf (tp-upper ret) (tp-upper t2))
	   (prop-bounds ret))
	  ((first-p t1)
	   (setf ret (insert-timepoint-before t2))
	   (setf (tp-inc ret) (list t1))
	   (push ret (tp-out t1))
	   (setf (tp-lower ret) (tp-lower t1))
	   (prop-bounds t1))
	  (t
		(setf ret (make-timepoint
					:in (list t1)
					:out (list t2)
					:lower (tp-lower t1)
					:upper (tp-upper t2)))
		(push ret (tp-inc t2))
		(push ret (tp-out t1))
		(prop-bounds ret)))))

(defun get-relation-helper (src dst seen)
  (cond
	((and (equal (tp-chain src) (tp-chain dst)))
	 (<= (tp-ptime src) (tp-ptime dst)))
	((not (gethash src seen))
	 (setf (gethash src seen) t)
	 (dolist (node (get-successors src))
	   (if (get-relation-helper node dst seen)
		 t)))))

;;; For two timepoints t1 and t2, compute the relation (if one exists)
;;; between the two timepoints. Possible return values are:
;;; 	- nil : no relation found
;;; 	- 1   : t1 before t2
;;;     - 2   : t1 after t2 (t2 before t1)
;;;     - 3   : t1 equal to t2
(defun get-relation (t1 t2)
  (cond
	((or (not t1) (not t2)) nil)
	((and (equal (tp-chain t1) (tp-chain t2)) 
		  (< (tp-ptime t1) (tp-ptime t2))) 1)
	((and (equal (tp-chain t1) (tp-chain t2)) 
		  (> (tp-ptime t1) (tp-ptime t2))) 2)
	((equal t1 t2) 3)
	((get-relation-helper t1 t2 (make-hash-table :test #'equal)) 1)
	((get-relation-helper t2 t1 (make-hash-table :test #'equal)) 2)
	(t nil)))

(defun insert-lower-bound (t1 bound)
  (progn
	(setf (tp-lower t1) bound)
	(prop-lower-bound t1)))

(defun insert-upper-bound (t1 bound)
  (progn
	(setf (tp-upper t1) bound)
	(prop-upper-bound t1)))

;;; Propogate lower bound
(defun prop-lower-bound (t1)
  (dolist (tk (get-successors t1))
	(prop-bound-down tk  (tp-lower t1))))

;;; Propogate upper bound
(defun prop-upper-bound (t1)
  (dolist (tk (get-ancestors t1))
	(prop-bound-up tk (tp-upper t1))))

(defun prop-bounds (t1)
  (progn
	(prop-upper-bound t1)
	(prop-lower-bound t1)))

;;; Propogate timebounds up
(defun prop-bound-down (t1 bound)
  (cond 
	((not t1) nil)
	((or (not (tp-lower t1)) (< (tp-lower t1) bound))
	 (setf (tp-lower t1) bound)
	 (dolist (tk (get-successors t1)) 
	   (prop-bound-down tk bound)))))

;;; Propogate timebounds down
(defun prop-bound-up (t1 bound)
  (cond
	((not t1) nil)
	((or (not (tp-upper t1)) (> (tp-upper t1) bound))
	 (setf (tp-upper t1) bound)
	 (dolist (tk (get-ancestors t1)) 
	   (prop-bound-up tk bound)))))

;;; testing functions
;;; -----------------------------------------------------------------------

(defun print-tp (tp)
  (format t "prev: ~A~%next: ~A~%links: ~A"
  (tp-prev tp) (tp-next tp) (tp-out tp)))
