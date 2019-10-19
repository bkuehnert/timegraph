;;; Directed graph is a list of payloads (called nodes) together with 
;;; a list of incoming and outgoing edges.
(defclass digraph ()
  ((nodes :initarg :nodes 
		  :accessor dg-nodes)
   (outgoing :initarg :outgoing 
			 :accessor dg-out)
   (incoming :initarg :incoming 
			 :accessor dg-inc)
   ))

;;; Create and return an empty directed graph.
(defun make-digraph ()
  (make-instance 'digraph
				 :nodes nil
				 :outgoing (make-hash-table
							 :test #'equal)
				 :incoming (make-hash-table
							 :test #'equal)
				 
				))

;;; Insert node into graph
(defun insert-node (digraph node)
  (setf (dg-nodes digraph) (cons node (dg-nodes digraph))))

;;; Insert edge into graph
(defun insert-edge (digraph org dst)
  (progn
	(setf (gethash org (dg-out digraph)) 
		  (cons dst (gethash org (dg-out digraph))))
	(setf (gethash dst (dg-inc digraph))
		  (cons org (gethash dst (dg-inc digraph))))))

;;; Print function
(defun print-digraph (digraph)
  (loop for x in (dg-nodes digraph)
		do (format t "~A: ~A ~%" x 
				   (gethash x (dg-out digraph)))))
