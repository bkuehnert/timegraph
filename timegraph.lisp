(load "graph.lisp")
(load "macro.lisp")

;;; Time graph is composed of two directed acyclic graphs (DAGs), and a map
;;; which relates the two
;;; -----------------------------------------------------------------------
;;; * hash: a hashtable which maps the name of a timepoint to the object
;;; which stores the information about the timepoint. Timepoints with
;;; the 'equal' relation are hashed to the same object.
;;;
;;; * chains: is hashtable which hashes timepoints in the same chain
;;; together.

(defclass timegraph ()
   ((hash :initarg :hash
		 :accessor tg-hash)
   (chains :initarg :chains
		   :accessor tg-chains)
   ))

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
  ((prev :initarg :prev
		 :accessor tp-prev)
   (next :initarg :next
		 :accessor tp-next)
   (ptime :initarg :ptime
		  :accessor tp-ptime)
   (links :initarg :links
		  :accessor tp-links)))


;;; Create and return an empty timegraph.
(defun make-timegraph ()
  (make-instance 'timegraph
				 :hash (make-hash-table :test #'equal)
				 :chains (make-hash-table :test #'equal)))

;;; Create and return a new timepoint
(defun make-timepoint (prev next ptime links)
  (make-instance 'timepoint
				 :prev prev
				 :next next
				 :ptime ptime
				 :links links))


;;; Function to get list of keys in hashtable.
(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

;;; Check if a timepoint t1 is the last in its chain
(defun last-p (t1)
  (not (tp-next t1)))

;;; Check if a timepoint t1 is the first in its chain
(defun first-p (t1)
  (not (tp-prev t1)))

;;; Helper function: For a timepoint t1, incremement the pseudotime by 1 of
;;; t1 and all timepoints in the chain after t1.
;(defun renumber-chain (tgraph t1)
;  (let ((table (chain-ptime (gethash t1 (tg-chains tgraph)))))
;	  (maphash #'(lambda (key val)
;				   (if (>= val (gethash t1 table))
;					 (setf (gethash key table) (+ 1 val))))
;			   table)))

;;; Introduce a new timepoint into the timegraph with no relations. This 
;;; puts the new timepoint in its own chain. 
(defun insert-timepoint (tgraph t1)
  (let ((tp (make-timepoint nil nil 1 nil)))
	  (progn
		(setf (gethash t1 (tg-hash tgraph)) tp)
		(setf (gethash tp (tg-chains tgraph)) (sxhash tp)))))

;;; Insert a new timepoint, t1, into the graph that is after some existing 
;;; point t2. Note: 't2' is not the name of a timepoint, but instead is 
;;; the actual timepoint object itself.
(defun insert-timepoint-after (tgraph t1 t2)
  (progn
	(if (last-p t2)
	  (progn 
	    (setf (gethash t1 (tg-hash tgraph)) 
			  (make-timepoint t2 nil (+ (tp-ptime t2) 1) nil))
	    (setf (gethash (gethash t1 (tg-hash tgraph))
					   (tg-chains tgraph)) 
			  (gethash t2 (tg-chains tgraph)))
		(setf (tp-next t2) (gethash t1 (tg-hash tgraph))))
	  (progn
		(insert-timepoint tgraph t1)
		(setf (tp-links t2) (cons (gethash t1 (tg-hash tgraph))
								  (tp-links t2)))))))

;;; Insert a new timepoint, t1, into the  graph that is before some 
;;; existing point t2. Note: 't2' is not the name of a timepoint, but
;;; instead is the actual timepoint object itself.
(defun insert-timepoint-before (tgraph t1 t2)
  (progn
	(if (first-p t2)
	  (progn
	    (setf (gethash t1 (tg-hash tgraph)) 
			  (make-timepoint nil t2 (- (tp-ptime t2) 1) nil))
	    (setf (gethash (gethash t1 (tg-hash tgraph)) (tg-chains tgraph)) 
			  (gethash t2 (tg-chains tgraph)))
		(setf (tp-prev t2) (gethash t1 (tg-hash tgraph))))
	  (progn
	    (insert-timepoint tgraph t1)
	    (setf (tp-links (gethash t1 (tg-hash tgraph))) 
			  (cons t2 nil))))))

;;; Insert a new timepoint, t1, into the graph that is equal to some
;;; existing timepoint t2. Note: 't2' **IS** the name of the timepoint
;;; this function ensures that both timepoints hash to the same object.
(defun insert-timepoint-equal (tgraph t1 t2)
  (setf (gethash t1 (tg-hash tgraph)) (gethash t2 (tg-hash tgraph))))

;;; Insert a new time point, t1, into the graph that is between some
;;; existing timepoints t2 and t3. 
;;; note: may cause inefficiency as the link t1 -> t3 remains intact. It
;;; may be better t oremove this link in some cases to reduce redundancy
;;; in querying.
(defun insert-timepoint-during (tgraph t1 t2 t3)
  (let ((chain2 (gethash (gethash t2 (tg-hash tgraph)) 
						 (tg-chains tgraph)))
		(chain3 (gethash (gethash t3 (tg-hash tgraph))
						 (tg-chains tgraph)))
		(t2 (gethash t2 (tg-hash tgraph)))
		(t3 (gethash t3 (tg-hash tgraph))))
	(progn
	  (cond 
		((and (equal chain2 chain3) (equal (tp-next t2) t3))
		 (progn
	      (setf (gethash t1 (tg-hash tgraph)) 
				(make-timepoint 
				  t2 
				  t3 
				  (/ (+ (tp-ptime t2) (tp-ptime t3)) 2) nil))
		  (setf (gethash (gethash t1 (tg-hash tgraph)) 
						 (tg-chains tgraph)) chain2)
		  (setf (tp-next t2) (gethash t1 (tg-hash tgraph)))
		  (setf (tp-prev t3) (gethash t1 (tg-hash tgraph)))))
		((last-p t2) 
		 (progn
		   (insert-timepoint-after tgraph t1 t2)
		   (setf (tp-links (gethash t1 (tg-hash tgraph)))
				 (cons t3 (tp-links (gethash t1 (tg-hash tgraph)))))))
		((first-p t3) 
		 (progn
		   (insert-timepoint-before tgraph t1 t3)
		   (setf (tp-links t2)
				 (cons (gethash t1 (tg-hash tgraph)) (tp-links t2)))))
		(t 
		  (progn
			(insert-timepoint tgraph t1)
		    (setf (tp-links (gethash t1 (tg-hash tgraph)))
				  (cons t3 (tp-links (gethash t1 (tg-hash tgraph))))))
		    (setf (tp-links t2)
				  (cons (gethash t1 (tg-hash tgraph)) (tp-links t2))))))))

;;; runs through src's chain and runs helper2
(defun get-relation-helper1 (tgraph src dst seen)
  (let ((chain-src (gethash src (tg-chains tgraph)))
		(chain-dst (gethash dst (tg-chains tgraph))))
	(cond 
	  ((and (equal chain-src chain-dst)
				   (<= (tp-ptime src) (tp-ptime dst))) t)
	  ((and (equal chain-src chain-dst)
				   (> (tp-ptime src) (tp-ptime dst))) nil)
	  ((not (gethash src seen)) 
	   (progn
		 (setf (gethash src seen) t)
		 (funcall (alambda (tgraph1 src1 dst1 seen1) 
		   (if src1 
			 (if (get-relation-helper2 tgraph1 src1 dst1 seen1) 
			   t
			   (self tgraph1 (tp-next src1) dst1 seen1)))) 
				  tgraph src dst seen))))))

;;; runs through src's cross-chain links and runs helper1
(defun get-relation-helper2 (tgraph src dst seen)
  (progn
	(setf (gethash src seen) t)
    (dolist (node (tp-links src))
	  (if (get-relation-helper1 tgraph node dst seen) (return t)))))

;;; For two timepoints t1 and t2, compute the relation (if one exists)
;;; between the two timepoints. Possible return values are:
;;; 	- nil : no relation found
;;; 	- 1   : t1 before t2
;;;     - 2   : t1 after t2 (t2 before t1)
;;;     - 3   : t1 equal to t2
(defun get-relation (tgraph t1 t2)
  (let ((chain1 (gethash (gethash t1 (tg-hash tgraph)) 
						 (tg-chains tgraph)))
		(chain2 (gethash (gethash t2 (tg-hash tgraph))
						 (tg-chains tgraph)))
 		(t1 (gethash t1 (tg-hash tgraph)))
		(t2 (gethash t2 (tg-hash tgraph))))
	(cond
	  ((or (not t1) (not t2)) nil)
	  ((and (equal chain1 chain2) (< (tp-ptime t1) (tp-ptime t2))) 1)
	  ((and (equal chain1 chain2) (> (tp-ptime t1) (tp-ptime t2))) 2)
	  ((equal t1 t2) 3)
	  ((get-relation-helper1 
		 tgraph t1 t2 (make-hash-table :test #'equal)) 1)
	  ((get-relation-helper1
		 tgraph t2 t1 (make-hash-table :test #'equal)) 2)
	  (t nil))))

;;; testing functions
;;; ----------------------------------------------------------------------

(defun get-rep (timegraph t1)
  (gethash t1 (tg-hash timegraph)))

(defun print-tp (tp)
  (format t "prev: ~A~%next: ~A~%links: ~A"
  (tp-prev tp) (tp-next tp) (tp-links tp)))
