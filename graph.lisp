;;; Directed graph is a list of payloads (called nodes) together with 
;;; a list of incoming and outgoing edges.
(defclass digraph ()
  ((nodes :initarg :nodes :reader dg-nodes)
   (outgoing :initarg :outgoing :reader dg-outgoing)
   (incoming :initarg :incoming :reader dg-incoming)
   ))

;;; Create and return an empty directed graph.
(defun make-digraph ()
  (make-instance 'digraph
				 :nodes nil
				 :outgoing (make-hash-table
							 :test #'equal)
				 :outgoing (make-hash-table
							 :test #'equal)
				 
				))

;;; Insert node into graph
(defun insert-node (digraph node)
  (setf (dg-nodes digraph) (cons node (dg-nodes digraph))))

;;; Insert edge into graph
(defun insert-edge (digraph org dst)
  (progn
	(setf (gethash org (dg-outgoing digraph)) 
		  (cons dst (gethash org (dg-outgoing digraph))))
	(setf (gethash dst (dg-incoming digraph))
		  (cons org (gethash dst (dg-incoming digraph))))))

;;; Print function
(defun print-digraph (digraph)
  (loop for x in (dg-nodes digraph)
		do (format t "~A: ~A ~%" x 
				   (gethash x (dg-outgoing digraph)))))
