(load "timepoint.lisp")

;;; A timegraph is a pair of hashtable. The first hashtable resolves an
;;; episode symbol to the timepoint representing the start of the
;;; episode. Similarly, the second hashtable resolves an episode symbol
;;; to the timepoint representing the end of the episode.
(defun make-timegraph ()
  (list (make-hash-table :test #'equal)
		(make-hash-table :test #'equal)))

;;; Given timepoints t1 and t2, alter the graph so that any point tk such
;;; that there exists a t1 -> tk and a tk -> t2 path is set equal to t1.
;;; For any tk, the all incoming/outgoing edges are added as cross-chain
;;; links to t1. Furthermore, refs is added to t1 and t2, which is
;;; reflected in the timegraph tg.
(defun assert-tp-equal (tg t1 t2)
  (let* ((t1suc (get-all-successors t1))
		 (t2anc (get-all-ancestors t2))
		 (quo (intersection t1suc t2anc)))
	(dolist (tk quo)
	  (setf (tp-inc t1) (union (tp-inc t1) 
							   (remove-if (lambda (x) (member x quo)) 
										  (cons (tp-prev tk) (tp-inc tk)))))
	  (setf (tp-out t1) (union (tp-out t1) 
							   (remove-if (lambda (x) (member x quo)) 
										  (cons (tp-next tk) (tp-out tk)))))
	  (setf (tp-brefs t1) (union (tp-brefs t1) (tp-brefs tk)))
	  (setf (tp-erefs t1) (union (tp-erefs t1) (tp-erefs tk)))
	  (dolist (bref (tp-brefs tk))
		(setf (gethash bref (first tg)) t1))
	  (dolist (eref (tp-erefs tk))
		(setf (gethash eref (second tg)) t1)))))


(defun assert-consec (tg e1 e2)
  (cond
	((and (not (gethash e1 (first tg))) (not (gethash e2 (first tg))))
	 (let* ((t1 (make-timepoint :brefs (list e1)))
		   (t2 (insert-timepoint-after t1 :erefs (list e1) 
									      :brefs (list e2)))
		   (t3 (insert-timepoint-after t2 :erefs (list e2))))
	   (setf (gethash e1 (first tg)) t1)
	   (setf (gethash e1 (second tg)) t2)
	   (setf (gethash e2 (first tg)) t2)
	   (setf (gethash e2 (second tg)) t3)))
	((not (gethash e1 (first tg)))
	 (let* ((t2 (gethash e2 (first tg)))
		   (t1 (insert-timepoint-before t2 :brefs (list e1))))
	   (setf (gethash e1 (first tg)) t1)
	   (setf (gethash e1 (second tg)) t1)
	   (push e1 (tp-erefs t2))))
	((not (gethash e2 (first tg)))
	 (let* ((t2 (gethash e1 (second tg)))
		   (t3 (insert-timepoint-after t2 :erefs (list e2))))
	   (setf (gethash e2 (first tg)) t2)
	   (setf (gethash e2 (second tg)) t3)
	   (push e2 (tp-brefs t2))))
	(t 
	  (let ((t21 (gethash e1 (second tg)))
			(t22 (gethash e2 (first tg))))
		;;; ensure that t21 is before t22
		;;; write function so that t21 <- t22 and everything in between
		;;; has same refs as t21 union t22. 
		(assert-tp-equal tg t21 t22)))))

;;; (defun assert-prior (tg e1 e2))

