(load "timepoint.lisp")

;;; A timegraph is a pair of hashtable. The first hashtable resolves an
;;; episode symbol to the timepoint representing the start of the
;;; episode. Similarly, the second hashtable resolves an episode symbol
;;; to the timepoint representing the end of the episode.
(defun make-timegraph ()
  (list (make-hash-table :test #'equal)
		(make-hash-table :test #'equal)))

; Assertion Functions
; -------------------------------------------------------------------------

;;; For two epsiodes e1 and e2, assert that
;;; e1.st <= e1.end = e2.st <= e2.end
(defun ep-assert-consec (tg e1 e2)
  (let* ((pair1 (tp-assert-before (gethash e1 (first tg))
							      (gethash e1 (second tg))))
		 (pair2 (tp-assert-equals tg
								  (second pair1)
								  (gethash e2 (first tg))))
		 (pair3 (tp-assert-before (second pair2)
								  (gethash e2 (second tg))))
		 (t1 (first pair1))
		 (t2 (first pair2))
		 (t3 (second pair3)))
	(setf (gethash e1 (first tg)) t1)
	(setf (gethash e2 (first tg)) t2)
	(setf (gethash e1 (second tg)) t2)
	(setf (gethash e2 (second tg)) t3)
	(push e1 (tp-brefs t1))
	(push e2 (tp-brefs t2))
	(push e1 (tp-erefs t2))
	(push e2 (tp-erefs t3))))

;;; For two epsiodes e1 and e2, assert that
;;; e1.st = e2.st && e2.end = e2.end
(defun ep-assert-equals (tg e1 e2)
  (let* ((pair1 (tp-assert-equals (gethash e1 (first tg))
								  tg
								  (gethash e2 (first tg))))
		 (pair2 (tp-assert-equals tg
								  (gethash e1 (second tg))
								  (gethash e2 (second tg))))
		 (t1 (first pair1))
		 (t2 (first pair2)))
	(setf (gethash e1 (first tg)) t1)
	(setf (gethash e2 (first tg)) t1)
	(setf (gethash e1 (second tg)) t2)
	(setf (gethash e2 (second tg)) t2)
	(push e1 (tp-brefs t1))
	(push e2 (tp-brefs t1))
	(push e1 (tp-erefs t2))
	(push e2 (tp-erefs t2))))

;;; For two epsiodes e1 and e2, assert that
;;; e1.st <= e2.st && e1.st <= e1.end && e2.st <= e2.end
(defun ep-assert-before (tg e1 e2)
  (let* ((pair1 (tp-assert-before (gethash e1 (first tg))
								  (gethash e2 (first tg))))
		 (pair2 (tp-assert-before (first pair1)
								  (gethash e1 (second tg))))
		 (pair3 (tp-assert-before (second pair1)
								  (gethash e2 (second tg))))
		 (t1 (first pair2))
		 (t2 (second pair2))
		 (t3 (first pair3))
		 (t4 (second pair3)))

	(setf (gethash e1 (first tg)) t1)
	(setf (gethash e2 (first tg)) t3)
	(setf (gethash e1 (second tg)) t2)
	(setf (gethash e2 (second tg)) t4)
	(push e1 (tp-brefs t1))
	(push e2 (tp-brefs t3))
	(push e1 (tp-erefs t2))
	(push e2 (tp-erefs t4))))

;;; For two epsiodes e1 and e2, assert that
;;; e2.st <= e1.st <= e1.end <= e2.end
(defun ep-assert-at-about (tg e1 e2)
  (let* ((pair1 (tp-assert-before (gethash e2 (first tg))
								  (gethash e1 (first tg))))
		 (pair2 (tp-assert-before (second pair1)
								  (gethash e1 (second tg))))
		 (pair3 (tp-assert-before (second pair2)
								  (gethash e2 (second tg))))
		 (t1 (first pair1))
		 (t2 (first pair2))
		 (t3 (first pair3))
		 (t4 (second pair3)))

	(setf (gethash e1 (first tg)) t2)
	(setf (gethash e2 (first tg)) t1)
	(setf (gethash e1 (second tg)) t3)
	(setf (gethash e2 (second tg)) t4)
	(push e1 (tp-brefs t2))
	(push e2 (tp-brefs t1))
	(push e1 (tp-erefs t3))
	(push e2 (tp-erefs t4))))

;;; For two epsiodes e1 and e2, assert that
;;; e1.st <= e2.end <= e2.st <= e2.end
(defun ep-assert-precond-of (tg e1 e2)
  (let* ((pair1 (tp-assert-before (gethash e1 (first tg))
							      (gethash e1 (second tg))))
		 (pair2 (tp-assert-before (second pair1)
								  (gethash e2 (first tg))))
		 (pair3 (tp-assert-before (second pair2)
								  (gethash e2 (second tg))))
		 (t1 (first pair1))
		 (t2 (first pair2))
		 (t3 (second pair2))
		 (t4 (second pair3)))

	(setf (gethash e1 (first tg)) t1)
	(setf (gethash e2 (first tg)) t2)
	(setf (gethash e1 (second tg)) t3)
	(setf (gethash e2 (second tg)) t4)
	(push e1 (tp-brefs t1))
	(push e2 (tp-brefs t3))
	(push e1 (tp-erefs t2))
	(push e2 (tp-erefs t4))))


; Querying functions
; -------------------------------------------------------------------------

(defun ep-consec-p (tg e1 e2)
  (let ((e1str (gethash e1 (first tg)))
		(e1end (gethash e1 (second tg)))
		(e2str (gethash e2 (first tg)))
		(e2end (gethash e2 (second tg))))

	(and (tp-before-p e1str e1end)
		 (tp-equals-p e1end e2str)
		 (tp-before-p e2str e2end))))

(defun ep-not-consec-p (tg e1 e2)
  (let ((e1str (gethash e1 (first tg)))
		(e1end (gethash e1 (second tg)))
		(e2str (gethash e2 (first tg)))
		(e2end (gethash e2 (second tg))))

	(or (tp-not-before-p e1str e1end)
		(tp-not-equals-p e1end e2str)
		(tp-not-before-p e2str e2end))))

(defun ep-equals-p (tg e1 e2)
  (let ((e1str (gethash e1 (first tg)))
		(e1end (gethash e1 (second tg)))
		(e2str (gethash e2 (first tg)))
		(e2end (gethash e2 (second tg))))

	(and (tp-equals-p e1str e2str)
		 (tp-equals-p e2end e2end))))

(defun ep-not-equals-p (tg e1 e2)
  (let ((e1str (gethash e1 (first tg)))
		(e1end (gethash e1 (second tg)))
		(e2str (gethash e2 (first tg)))
		(e2end (gethash e2 (second tg))))

	(or (tp-not-equals-p e1str e2str)
		(tp-not-equals-p e1end e2end))))

(defun ep-before-p (tg e1 e2)
  (let ((e1str (gethash e1 (first tg)))
		(e1end (gethash e1 (second tg)))
		(e2str (gethash e2 (first tg)))
		(e2end (gethash e2 (second tg))))

	(and (tp-before-p e1str e2str)
		 (tp-before-p e1str e1end)
		 (tp-before-p e2str e2end))))

(defun ep-not-before-p (tg e1 e2)
  (let ((e1str (gethash e1 (first tg)))
		(e1end (gethash e1 (second tg)))
		(e2str (gethash e2 (first tg)))
		(e2end (gethash e2 (second tg))))

	(or (tp-not-before-p e1str e2str)
		(tp-not-before-p e1str e1end)
		(tp-not-before-p e2str e2end))))

(defun ep-at-about-p (tg e1 e2)
  (let ((e1str (gethash e1 (first tg)))
		(e1end (gethash e1 (second tg)))
		(e2str (gethash e2 (first tg)))
		(e2end (gethash e2 (second tg))))

	(and (tp-before-p e2str e1str)
		 (tp-before-p e1str e1end)
		 (tp-before-p e1end e2end))))

(defun ep-not-at-about-p (tg e1 e2)
  (let ((e1str (gethash e1 (first tg)))
		(e1end (gethash e1 (second tg)))
		(e2str (gethash e2 (first tg)))
		(e2end (gethash e2 (second tg))))

	(or (tp-not-before-p e2str e1str)
		(tp-not-before-p e1str e1end)
		(tp-not-before-p e1end e2end))))

(defun ep-precond-of-p (tg e1 e2)
  (let ((e1str (gethash e1 (first tg)))
		(e1end (gethash e1 (second tg)))
		(e2str (gethash e2 (first tg)))
		(e2end (gethash e2 (second tg))))

	(and (tp-before-p e1str e1end)
		 (tp-before-p e1end e2str)
		 (tp-before-p e2str e2end))))

(defun ep-not-precond-of-p (tg e1 e2)
  (let ((e1str (gethash e1 (first tg)))
		(e1end (gethash e1 (second tg)))
		(e2str (gethash e2 (first tg)))
		(e2end (gethash e2 (second tg))))

	(or (tp-not-before-p e1str e1end)
		(tp-not-before-p e1end e2str)
		(tp-not-before-p e2str e2end))))

