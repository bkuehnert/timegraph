(load "graph.lisp")

;;; Time graph is composed of two directed acyclic graphs (DAGs), and a map
;;; which relates the two
;;; -----------------------------------------------------------------------
;;; * dag:, consists of time points with an edge (u,v) meaning that u 
;;; occurs before v. 
;;;
;;; * meta: the metagraph which is 'dag' modulo timechains, which is a list 
;;; of time points in successive order. 
;;;
;;; * chains: is hashmap which maps each each timepoint to its chain
;;;
;;; * K: is the number of chains in the metagraph. Used for assigning new 
;;; chains to their 'chain number'. (note: may be deprecated)

(defclass timegraph ()
  ((dag :initarg :dag
		:accessor tg-dag)
   (meta :initarg :meta
		 :accessor tg-meta)
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
				 :chains (make-hash-table :test #'equal)
				 :K 0))

;;; Create and return an empty chain with chain number K.
(defun make-chain (K)
  (make-instance 'chain
				 :num K
				 :top nil
				 :bot nil
				 :ptime (make-hash-table :test #'equal)))

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
		(setf (tg-K tgraph) (+ (tg-K tgraph) 1))
		(insert-node (tg-meta tgraph) chain)
		(setf (chain-top chain) t1)
		(setf (chain-bot chain) t1)
		(setf (gethash t1 (chain-ptime chain)) 1)
		(setf (gethash t1 (tg-chains tgraph)) chain))))

;;; Insert a new timepoint, t1, into the graph that is after some existing 
;;; point t2.
(defun insert-timepoint-after (tgraph t1 t2)
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
			(insert-edge (tg-meta tgraph) 
				(gethash t2 (tg-chains tgraph))
				(gethash t1 (tg-chains tgraph)))))
	(insert-node (tg-dag tgraph) t1)
	(insert-edge (tg-dag tgraph) t2 t1)
  ))

;;; Insert a new timepoint, t1, into the  graph that is before some existing
;;; point t2
(defun insert-timepoint-before (tgraph t1 t2)
  (progn
  	(if (first-p tgraph t2)
		(let ((chain (gethash t2 (tg-chains tgraph))))
			(progn
			  	(setf (chain-top chain) t1)
	   			(setf (gethash t1 (tg-chains tgraph)) chain)
   				(setf (gethash t1 (chain-ptime chain))
					  (- 1 (gethash t2 (chain-ptime chain))))))
	 	 (progn
			(insert-timepoint tgraph t1)
			(insert-edge (tg-meta tgraph) 
				(gethash t1 (tg-chains tgraph))
				(gethash t2 (tg-chains tgraph)))))
	(insert-node (tg-dag tgraph) t1)
	(insert-edge (tg-dag tgraph) t1 t2)
  ))
