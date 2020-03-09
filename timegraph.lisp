(load "timepoint.lisp")

;;; A timegraph is a pair of hashtable. The first hashtable resolves an
;;; episode symbol to the timepoint representing the start of the
;;; episode. Similarly, the second hashtable resolves an episode symbol
;;; to the timepoint representing the end of the episode.
(defun make-timegraph ()
  (list (make-hash-table :test #'equal)
		(make-hash-table :test #'equal)))

(defun assert-consec (tg e1 e2)
  (let* ((pair1 (assert-before (gethash e1 (first tg))
							   (gethash e1 (second tg))))
		 (pair2 (assert-before (second pair1)
							   (gethash e2 (second tg)))))


(defun assert-consec (tg e1 e2)
  (let ((t11 (gethash e1 (first tg)))
	(t12 (gethash e1 (second tg)))
	(t21 (gethash e2 (first tg)))
	(t22 (gethash e2 (second tg))))
    (cond
      ((and (not t11) (not t21))
       (let* ((t1 (make-timepoint :brefs (list e1)))
	      (t2 (insert-timepoint-after t1
					  :erefs (list e1)
					  :brefs (list e2)))
	      (t3 (insert-timepoint-after t2
					  :erefs (list e2))))
	 (setf (gethash e1 (first tg)) t1)
	 (setf (gethash e1 (second tg)) t2)
	 (setf (gethash e2 (first tg)) t2)
	 (setf (gethash e2 (second tg)) t3)))

      ((not t11)
       (let ((t1 (insert-timepoint-before t21
					  :brefs (list e1))))
	 (setf (gethash e1 (first tg)) t1)
	 (setf (gethash e1 (second tg)) t21)
	 (push e1 (tp-erefs t21))))

      ((not t21)
       (let* ((t3 (insert-timepoint-after t12
					  :erefs (list e2))))
	    (setf (gethash e2 (first tg)) t12)
	    (setf (gethash e2 (second tg)) t3)
	    (push e2 (tp-brefs t12))))

      ((equal t12 t21) t)

      ((tg-before-p t12 t21)
       (tp-assert-equal tg t12 t21))

      ((tg-before-p t21 t12)
       (tp-assert-equal tg t21 t12))

      (t
       (tp-assert-before t12 t21)
       (tp-assert-equal tg t12 t21)))))
      

(defun assert-prior (tg e1 e2)
  (let ((t11 (gethash e1 (first tg)))
	(t12 (gethash e1 (second tg)))
	(t21 (gethash e2 (first tg)))
	(t22 (gethash e2 (second tg))))
    (cond
      ((and (not t11) (not t21))
       (let* ((t11 (make-timepoint :brefs (list e1)))
	      (t12 (insert-timepoint-after t11 :erefs (list e1)))
	      (t21 (insert-timepoint-after t12 :brefs (list e2)))
	      (t21 (insert-timepoint-after t21 :erefs (list e2))))
	 (setf (gethash e1 (first tg)) t11)
	 (setf (gethash e1 (second tg)) t12)
	 (setf (gethash e2 (first tg)) t21)
	 (setf (gethash e2 (second tg)) t22)))

      ((not t11)
       (let* ((t12 (insert-timepoint-before t21
					    :erefs (list e1)))
	      (t11 (insert-timepoint-before t12
					    :brefs (list e1)))))
	 (setf (gethash e1 (first tg)) t11)
	 (setf (gethash e1 (second tg)) t12))


      ((not t21)
       (let* ((t21 (insert-timepoint-after t21
					    :brefs (list e2)))
	      (t22 (insert-timepoint-before t21
					    :1refs (list e2)))))
	 (setf (gethash e2 (first tg)) t21)
	 (setf (gethash e2 (second tg)) t22))

      (t
       (tp-assert-before t12 t21)))))
      
