(load "graph.lisp")

;;; Time graph is composed of two directed acyclic graphs (DAGs), and a map
;;; which relates the two
;;; -----------------------------------------------------------------------
;;; * dag:, consists of (uniquely determined) time points with an edge 
;;; (u,v) meaning that u occurs before v. 
;;;
;;; * meta: consists of (uniquely determined) time points with an edge
;;; (u,v) meaning that there is cross-chain link u -> v.
;;;
;;; * hash: a hashtable which maps each timepoint to an identifier. Two
;;; timepoints will have the same identifier if the timepoints have been
;;; set to be equal.
;;;
;;; * chains: is hashtable which maps each each timepoint to its chain
;;;
;;; * K: is the number of chains in the metagraph. Used for assigning new 
;;; chains to their 'chain number'. (note: may be deprecated)

(defclass timegraph ()
  ((dag :initarg :dag
		:accessor tg-dag)
   (meta :initarg :meta
		 :accessor tg-meta)
   (hash :initarg :hash
		 :accessor tg-hash)
   (chains :initarg :chains
		   :accessor tg-chains)
   (K :initarg :K 
	  :accessor tg-K)
   ))

;;; A chain stores all of the local data relevant to timepoints
;;; ----------------------------------------------------------------------
;;; * num: the number of the chain, used for identifying a chain
;;;
;;; * top: the timepoint at the top of the chain.
;;;
;;; * bot: the timepoint at the bottom of the chain.
;;;
;;; * ptime: a map which maps each timepoint to its pseudotime, a number
;;; used for making relative judgements within the chain.


(defclass chain ()
	((num :initarg :num
	   :accessor chain-num
	   )
	 (top :initarg :top
		  :accessor chain-top)
	 (bot :initarg :bot
		  :accessor chain-bot)
	 (ptime :initarg :ptime
			:accessor chain-ptime)))

;;; Create and return an empty timegraph.
(defun make-timegraph ()
  (make-instance 'timegraph
				 :dag (make-digraph)
				 :meta (make-digraph)
				 :hash (make-hash-table :test #'equal)
				 :chains (make-hash-table :test #'equal)
				 :K 0))

;;; Create and return an empty chain with chain number K.
(defun make-chain (K)
  (make-instance 'chain
				 :num K
				 :top nil
				 :bot nil
				 :ptime (make-hash-table :test #'equal)))

;;; Function to get list of keys in hashtable.
(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

;;; Check if a timepoint t1 is the last in its chain
(defun last-p (tgraph t1)
  (equal t1 (chain-bot (gethash t1 (tg-chains tgraph)))))

;;; Check if a timepoint t1 is the first in its chain
(defun first-p (tgraph t1)
  (equal t1 (chain-top (gethash t1 (tg-chains tgraph)))))

;;; Helper function: For a timepoint t1, incremement the pseudotime by 1 of
;;; t1 and all timepoints in the chain after t1.
(defun renumber-chain (tgraph t1)
  (let ((table (chain-ptime (gethash t1 (tg-chains tgraph)))))
	  (maphash #'(lambda (key val)
				   (if (>= val (gethash t1 table))
					 (setf (gethash key table) (+ 1 val))))
			   table)))

;;; Introduce a new timepoint into the timegraph with no relations. This 
;;; puts the new timepoint in its own chain. 
(defun insert-timepoint (tgraph t1)
  (let ((chain (make-chain (tg-K tgraph))))
	  (progn
		(insert-node (tg-dag tgraph) t1)
		(setf (gethash t1 (tg-hash tgraph)) t1)
		(setf (tg-K tgraph) (+ (tg-K tgraph) 1))
		(insert-node (tg-meta tgraph) chain)
		(setf (chain-top chain) t1)
		(setf (chain-bot chain) t1)
		(setf (gethash t1 (chain-ptime chain)) 1)
		(setf (gethash t1 (tg-chains tgraph)) chain))))

;;; Insert a new timepoint, t1, into the graph that is after some existing 
;;; point t2.
(defun insert-timepoint-after (tgraph t1 t2)
  (let ((t2 (gethash t2 (tg-hash tgraph))))
	(progn
	  (if (last-p tgraph t2)
		(let ((chain (gethash t2 (tg-chains tgraph))))
			(progn
			  	(setf (chain-bot chain) t1)
	   			(setf (gethash t1 (tg-chains tgraph)) chain)
   				(setf (gethash t1 (chain-ptime chain))
					  (+ 1 (gethash t2 (chain-ptime chain))))))
		(progn
		  (insert-timepoint tgraph t1)
		  (insert-edge (tg-meta tgraph) t2 t1)))
	  (setf (gethash t1 (tg-hash tgraph)) t1)
	  (insert-node (tg-dag tgraph) t1)
	  (insert-edge (tg-dag tgraph) t2 t1))))

;;; Insert a new timepoint, t1, into the  graph that is before some existing
;;; point t2
(defun insert-timepoint-before (tgraph t1 t2)
  (let ((t2 (gethash t2 (tg-hash tgraph))))
	(progn
	  (if (first-p tgraph t2)
		(let ((chain (gethash t2 (tg-chains tgraph))))
			(progn
			  	(setf (chain-top chain) t1)
	   			(setf (gethash t1 (tg-chains tgraph)) chain)
   				(setf (gethash t1 (chain-ptime chain))
					  (- (gethash t2 (chain-ptime chain)) 1))))
		(progn
		  (insert-timepoint tgraph t1)
		  (insert-edge (tg-meta tgraph) t1 t2)))
	  (setf (gethash t1 (tg-hash tgraph)) t1)
	  (insert-node (tg-dag tgraph) t1)
	  (insert-edge (tg-dag tgraph) t1 t2))))

;;; Insert a new timepoint, t1, into the graph that is equal to some
;;; existing timepoint t2
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
		((equal chain2 chain3)
		 (progn
		   (setf (gethash t1 (tg-chains tgraph)) chain2)
		   (setf (gethash t1 (chain-ptime chain2)) 
				 (/ (+ (gethash t2 (chain-ptime chain2))
					   (gethash t3 (chain-ptime chain2))) 2))))
		((last-p tgraph t2) 
		 (progn
		   (insert-timepoint-after tgraph t1 t2)
		   (insert-edge (tg-meta tgraph) t1 t3)))
		((first-p tgraph t3) 
		 (progn
		   (insert-timepoint-before tgraph t1 t3)
		   (insert-edge (tg-meta tgraph) t2 t1)))
		(t 
		  (progn
			(insert-timepoint tgraph t1)
			(insert-edge (tg-meta tgraph) t2 t1)
			(insert-edge (tg-meta tgraph) t1 t3))))

	  (insert-node (tg-dag tgraph) t1)
	  (insert-edge (tg-dag tgraph) t2 t1)
	  (insert-edge (tg-dag tgraph) t1 t3))))


;;; runs through src's chain and runs helper2
(defun get-relation-helper1 (tgraph src dst seen)
  (let ((chain-src (gethash src (tg-chains tgraph)))
		(chain-dst (gethash dst (tg-chains tgraph))))
	(cond 
	  ((and (equal chain-src chain-dst)
				   (<= (gethash src (chain-ptime chain-src))
					   (gethash dst (chain-ptime chain-dst)))) t)
	  ((and (equal chain-src chain-dst)
				   (> (gethash src (chain-ptime chain-src))
					   (gethash dst (chain-ptime chain-dst)))) nil)
	  ((not (gethash src seen)) 
	   (progn
		 (setf (gethash src seen) t)
		 (if (not (hash-keys (chain-ptime chain-src)))
		   nil
		   (dolist (node (hash-keys (chain-ptime chain-src)))
			 (if (get-relation-helper2 tgraph node src dst seen)
			   (return t)))))))))

;;; runs through src's cross-chain links and runs helper1
(defun get-relation-helper2 (tgraph src par dst seen)
  (let ((chain (gethash src (tg-chains tgraph))))
	(if (<= (gethash par (chain-ptime chain)) 
				(gethash src (chain-ptime chain)))
	  (progn
		(setf (gethash src seen) t)
		(if (not (get-outgoing (tg-meta tgraph) src))
		  nil
		  (dolist (node (get-outgoing (tg-meta tgraph) src))
			(if (get-relation-helper1 tgraph node dst seen)
			  (return t))))))))

;;; For two timepoints t1 and t2, compute the relation (if one exists)
;;; between the two timepoints. Possible return values are:
;;; 	- nil : no relation found
;;; 	- 1   : t1 before t2
;;;     - 2   : t1 after t2 (t2 before t1)
;;;     - 3   : t1 equal to t2
;;; note: can be made slightly faster by keeping chains sorted
(defun get-relation (tgraph t1 t2)
  (let ((chain1 (gethash (gethash t1 (tg-hash tgraph)) 
						 (tg-chains tgraph)))
		(chain2 (gethash (gethash t2 (tg-hash tgraph))
						 (tg-chains tgraph)))
 		(t1 (gethash t1 (tg-hash tgraph)))
		(t2 (gethash t2 (tg-hash tgraph))))
	(cond
	  ((or (not t1) (not t2)) nil)
	  ((and (equal chain1 chain2) (< (gethash t1 (chain-ptime chain1))
									 (gethash t2 (chain-ptime chain2)))) 1)
	  ((and (equal chain1 chain2) (> (gethash t1 (chain-ptime chain1))
									 (gethash t2 (chain-ptime chain2)))) 2)
	  ((equal t1 t2) 3)
	  ((get-relation-helper1 
		 tgraph t1 t2 (make-hash-table :test #'equal)) 1)
	  ((get-relation-helper1
		 tgraph t2 t1 (make-hash-table :test #'equal)) 2)
	  (t nil))))
