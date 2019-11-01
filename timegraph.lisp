(load "graph.lisp")

;;; Time graph is composed of two directed acyclic graphs (DAGs) and a
;;; hashmap.
;;; -----------------------------------------------------------------------
;;; * dag:, consists of time points with an edge (u,v) meaning that u 
;;; occurs before v. 
;;;
;;; * meta: the metagraph which is 'dag' modulo timechains, which is a list ;;; of time points in successive order. 
;;;
;;; *'data': is a hashmap which assigns each time point to a tuple (s,t)
;;; where s is the chain number at t is pseudotime
;;;
;;; *'K': is the number of chains in the metagraph. Used for assigning new 
;;; chains to their 'chain number'.

(defclass timegraph ()
  ((dag :initarg :dag
		:accessor tg-dag)
   (meta :initarg :meta
		 :accessor tg-meta)
   (data :initarg :data
		 :accessor tg-data)
   (K :initarg :K 
	  :accessor tg-K)
   ))

(defun make-timegraph()
  (make-instance 'timegraph
				 :dag (make-digraph)
				 :meta (make-digraph)
				 :data (make-hash-table :test #'equal)
				 :K 0))

;;; Check if a timepoint t1 is the last in its chain
;;; note: can be constant time by keeping track of max pseudotime.
(defun last-p (tgraph t1)
  (every 
	(lambda (tj) 
	  (not (equal (gethash t1 (tg-data tgraph))
				  (gethash tj (tg-data tgraph)))))
	(get-outgoing (tg-dag tgraph) t1)))

;;; Check if a timepoint t1 is the first in its chain
;;; note: can be constant time by keeping track of min pseudotime.
(defun first-p (tgraph t1)
  (every 
	(lambda (tj) 
	  (not (equal (gethash t1 (tg-data tgraph))
				  (gethash tj (tg-data tgraph)))))
	(get-incoming (tg-dag tgraph) t1)))

;;; Introduce a new timepoint into the timegraph with no relations. This 
;;;puts the new timepoint in its own chain. 
(defun insert-timepoint (tgraph t1)
  (progn
	(insert-node (tg-dag tgraph) t1)
	(setf (tg-K tgraph) (+ (tg-K tgraph) 1))
	(insert-node (tg-meta tgraph) (tg-K tgraph))
	(setf (gethash t1 (tg-data tgraph)) (list (tg-K tgraph) 1))))

;;; Insert a new timepoint, t1, into the graph that is after some existing 
;;; point t2.
(defun insert-timepoint-after (tgraph t1 t2)
  (progn
	(insert-node (tg-dag tgraph) t1)
	(insert-edge (tg-dag tgraph) t2 t1)
  	((if (last-p t2)
	  (setf (gethash t1 (tg-data tgraph) (chain-data tgraph t2 1)))
	  (progn
		(insert-timepoint tgraph t1)
		(insert-edge (tg-meta tgraph) 
					 (nth 1 (gethash t1 (tg-data tgraph)))
					 (tg-K tgraph)))
  ))))

;;; Insert a new timepoint, t1, into the  graph that is before some existing
;;; point t2
(defun insert-timepoint-before (tgraph t1 t2)
  (progn
	(insert-node (tg-dag tgraph) t1)
	(insert-edge (tg-dag tgraph) t1 t2)
  	((if (first-p t2)
	  (setf (gethash t1 (tg-data tgraph) (chain-data tgraph t2 -1)))
	  (progn
		(insert-timepoint tgraph t1)
		(insert-edge (tg-meta tgraph) 
					 (tg-K tgraph)
					 (nth 1 (gethash t1 (tg-data tgraph)))))
  ))))

;;; Helper function: For a timepoint t1, returns (s,t+inc) where s is the
;;; chain of t1, t is the pseudotime, and inc is a parameter
(defun chain-data (tgraph t1 inc)
  (list (nth 1 (gethash t1 (tg-data tgraph)))
		(+ inc (nth 2 (gethash t1 (tg-data tgraph))))))
