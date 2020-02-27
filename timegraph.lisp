(load "timepoint.lisp")

(defun make-timegraph ()
  (list (make-hash-table :test #'equal)
		(make-hash-table :test #'equal)))

(defun assert-consec (tg e1 e2)
  (cond 
	((and (not (gethash e1 (first tg))) (not (gethash e2 (first tg))))
	 (let* ((t1 (make-timepoint :brefs (list e1)))
			(t2 (insert-timepoint-after t1 :erefs (list e1) 
										   :brefs (list e2)))
			(t3 (insert-timepoint-after t2 :erefs (list e2))))
	   (setf (gethash e1 (first tg)) t1)
	   (setf (gethash e2 (first tg)) t2)
	   (setf (gethash e1 (second tg)) t2)
	   (setf (gethash e2 (second tg)) t3)))
	((not (gethash e1 (first tg)))
	 (let* ((t2 (gethash e2 (first tg)))
			(t1 (insert-timepoint-before t2 :erefs (list e1))))
	   (setf (tp-erefs t2) (union (tp-erefs t2) (list e1))) ; maybe too slow
	   (setf (gethash e1 (first tg)) t1)
	   (setf (gethash e1 (second tg)) t2)))
	((not (gethash e2 (first tg)))
	 (let* ((t2 (gethash e2 (second tg)))
			(t3 (insert-timepoint-after t2 :brefs (list e2))))
	   (setf (tp-brefs t2) (union (tp-brefs t2) (list e2))) ; maybe too slow
	   (setf (gethash e2 (first tg)) t2)
	   (setf (gethash e2 (second tg)) t3)))
	(t
	  (let* ((t21 (gethash e1 (second tg)))
			 (t22 (gethash e2 (first tg))))
		(assert-tp-equal (tg t21 t22))))))


