(load "timepoint.lisp")

(defvar *TG-CONSTRAINTS* (make-hash-table :test #'equal))

(defvar *trans* (make-hash-table :test #'equal))
(setf (gethash 't1 *trans*) `(get-str tg e1))
(setf (gethash 't2 *trans*) `(get-end tg e1))
(setf (gethash 't3 *trans*) `(get-str tg e2))
(setf (gethash 't4 *trans*) `(get-end tg e2))

;;; A timegraph is a pair of hashtable. The first hashtable resolves an
;;; episode symbol to the timepoint representing the start of the
;;; episode. Similarly, the second hashtable resolves an episode symbol
;;; to the timepoint representing the end of the episode.
(defclass timegraph ()
  ((str :initarg :str
		:accessor tg-str)
   (end :initarg :end
		:accessor tg-end)
;   (upper :initarg :upper
;		  :accessor tg-upper)
;   (lower :initarg :lower
;		  :accessor tg-lower)
))

(defun make-timegraph ()
  (make-instance 'timegraph
				 :str (make-hash-table :test #'equal)
				 :end (make-hash-table :test #'equal)
;				 :upper (make-hash-table :test #'equal)
;				 :lower (make-hash-table :test #'equal)
))

; Accessor functions
(defun get-str (tg e1)
  (gethash e1 (tg-str tg)))

(defun get-end (tg e1)
  (gethash e1 (tg-end tg)))

;(defun get-upper (tg p)
;  (gethash p (tg-upper tg)))

;(defun get-lower (tg p x)
;  (setf (gethash p (tg-lower tg)) x))

;(defun set-upper (tg p x)
;  (setf (gethash p (tg-upper tg)) x))

;(defun set-lower (tg p x)
;  (setf (gethash p (tg-lower tg)) x))

(defun set-str (tg e1 tp)
  (setf (gethash e1 (tg-str tg)) tp))

(defun set-end (tg e1 tp)
  (setf (gethash e1 (tg-end tg)) tp))


; Populating Constraints
; -------------------------------------------------------------------------

(defun create-ep-constraint (name conditions)
  (setf
	(gethash (intern name) *TG-CONSTRAINTS*)
	(list
	  (eval `(lambda (tg e1 e2)
		(let*
		  ,(append `((t1 (get-str tg e1))
					 (t2 (get-end tg e1))
					 (t3 (get-str tg e2))
					 (t4 (get-end tg e2)))
				   (apply #'append
					 (mapcar (lambda (con)
				       (cond
						 ; add an error check here
						 ((equal (first con) 0)
						  `((pair (tp-assert-equals tg ,(second con) ,(third con)))
							(,(second con) (first pair))
							(,(third con) (second pair))))
						 ((equal (first con) 1)
						  `((pair (tp-assert-before ,(second con) ,(third con)))
							(,(second con) (first pair))
							(,(third con) (second pair))))))
							 conditions)))

		  (set-str tg e1 t1)
		  (set-end tg e1 t2)
		  (set-str tg e2 t3)
		  (set-end tg e2 t4)
		  (setf (tp-brefs t1) (adjoin e1 (tp-brefs t1)))
		  (setf (tp-erefs t2) (adjoin e1 (tp-brefs t2)))
		  (setf (tp-brefs t3) (adjoin e2 (tp-brefs t3)))
		  (setf (tp-erefs t4) (adjoin e2 (tp-brefs t4))))))

	  (eval 
		`(lambda (tg e1 e2)
		   ,(append `(and)
			  (apply #'append
			    (mapcar (lambda (con)
				  (list (list
					(if (equal (first con) 0)
					  'tp-equals-p
					  'tp-before-p)
					(gethash (second con) *trans*)
					(gethash (third con) *trans*))))
				  conditions)))))

	  (eval 
		`(lambda (tg e1 e2)
		   ,(append `(or)
			  (apply #'append
			    (mapcar (lambda (con)
				  (list (list
					(if (equal (first con) 0)
					  'tp-not-equals-p
					  'tp-not-before-p)
					(gethash (second con) *trans*)
					(gethash (third con) *trans*))))
				  conditions))))))))

(create-ep-constraint "EQUALS" `((1 t1 t2) (0 t1 t3) (0 t2 t4)))
(create-ep-constraint "BEFORE" `((1 t1 t2) (1 t3 t4) (1 t1 t3)))
(create-ep-constraint "AFTER" `((1 t1 t2) (1 t3 t4) (1 t3 t1)))
(create-ep-constraint "CONSEC" `((1 t1 t2) (1 t3 t4) (0 t2 t3)))
(create-ep-constraint "AT-ABOUT" `((1 t1 t2) (1 t3 t4) (1 t3 t1) (1 t2 t4)))
(create-ep-constraint "PRECOND-OF" `((1 t1 t2) (1 t2 t3) (1 t3 t4)))
(create-ep-constraint "POSTCOND-OF" `((1 t1 t2) (1 t4 t1) (1 t3 t4)))

; Utility Functions
; -------------------------------------------------------------------------

(defun tg-time-prop-p (prop)
  (and 
	(listp prop)
	(equal (list-length prop) 3)
	(gethash (second prop) *TG-CONSTRAINTS*)))

(defun tg-assert-prop (prop tg)
  (funcall (first (gethash (second prop) *TG-CONSTRAINTS*))
		   tg (first prop) (third prop)))

(defun tg-eval-prop (prop tg)
  (let ((tuple (gethash (second prop) *TG-CONSTRAINTS*))
		(e1 (first prop))
		(e2 (third prop)))
	(cond
	  ((funcall (second tuple) tg e1 e2) 1)
	  ((funcall (third tuple) tg e1 e2) 0)
	  (t 2))))

;;; For two epsiodes e1 and e2, assert that
;;; e1.st <= e1.end = e2.st <= e2.end
;(defun ep-assert-consec (tg e1 e2)
;  (let* ((pair1 (tp-assert-before (gethash e1 (tg-str tg))
;							      (gethash e1 (tg-end tg))))
;		 (pair2 (tp-assert-equals tg
;								  (second pair1)
;								  (gethash e2 (tg-str tg))))
;		 (pair3 (tp-assert-before (second pair2)
;								  (gethash e2 (tg-end tg))))
;		 (t1 (first pair1))
;		 (t2 (first pair2))
;		 (t3 (second pair3)))
;	(setf (gethash e1 (tg-str tg)) t1)
;	(setf (gethash e2 (tg-str tg)) t2)
;	(setf (gethash e1 (tg-end tg)) t2)
;	(setf (gethash e2 (tg-end tg)) t3)
;	(push e1 (tp-brefs t1))
;	(push e2 (tp-brefs t2))
;	(push e1 (tp-erefs t2))
;	(push e2 (tp-erefs t3))))
;
;;;; For two epsiodes e1 and e2, assert that
;;;; e1.st = e2.st && e2.end = e2.end
;(defun ep-assert-equals (tg e1 e2)
;  (let* ((pair1 (tp-assert-equals (gethash e1 (tg-str tg))
;								  tg
;								  (gethash e2 (tg-str tg))))
;		 (pair2 (tp-assert-equals tg
;								  (gethash e1 (tg-end tg))
;								  (gethash e2 (tg-end tg))))
;		 (t1 (first pair1))
;		 (t2 (first pair2)))
;	(setf (gethash e1 (tg-str tg)) t1)
;	(setf (gethash e2 (tg-str tg)) t1)
;	(setf (gethash e1 (tg-end tg)) t2)
;	(setf (gethash e2 (tg-end tg)) t2)
;	(push e1 (tp-brefs t1))
;	(push e2 (tp-brefs t1))
;	(push e1 (tp-erefs t2))
;	(push e2 (tp-erefs t2))))
;
;;;; For two epsiodes e1 and e2, assert that
;;;; e1.st <= e2.st && e1.st <= e1.end && e2.st <= e2.end
;(defun ep-assert-before (tg e1 e2)
;  (let* ((pair1 (tp-assert-before (gethash e1 (tg-str tg))
;								  (gethash e2 (tg-str tg))))
;		 (pair2 (tp-assert-before (first pair1)
;								  (gethash e1 (tg-end tg))))
;		 (pair3 (tp-assert-before (second pair1)
;								  (gethash e2 (tg-end tg))))
;		 (t1 (first pair2))
;		 (t2 (second pair2))
;		 (t3 (first pair3))
;		 (t4 (second pair3)))
;
;	(setf (gethash e1 (tg-str tg)) t1)
;	(setf (gethash e2 (tg-str tg)) t3)
;	(setf (gethash e1 (tg-end tg)) t2)
;	(setf (gethash e2 (tg-end tg)) t4)
;	(push e1 (tp-brefs t1))
;	(push e2 (tp-brefs t3))
;	(push e1 (tp-erefs t2))
;	(push e2 (tp-erefs t4))))
;
;;;; For two epsiodes e1 and e2, assert that
;;;; e2.st <= e1.st <= e1.end <= e2.end
;(defun ep-assert-at-about (tg e1 e2)
;  (let* ((pair1 (tp-assert-before (gethash e2 (tg-str tg))
;								  (gethash e1 (tg-str tg))))
;		 (pair2 (tp-assert-before (second pair1)
;								  (gethash e1 (tg-end tg))))
;		 (pair3 (tp-assert-before (second pair2)
;								  (gethash e2 (tg-end tg))))
;		 (t1 (first pair1))
;		 (t2 (first pair2))
;		 (t3 (first pair3))
;		 (t4 (second pair3)))
;
;	(setf (gethash e1 (tg-str tg)) t2)
;	(setf (gethash e2 (tg-str tg)) t1)
;	(setf (gethash e1 (tg-end tg)) t3)
;	(setf (gethash e2 (tg-end tg)) t4)
;	(push e1 (tp-brefs t2))
;	(push e2 (tp-brefs t1))
;	(push e1 (tp-erefs t3))
;	(push e2 (tp-erefs t4))))
;
;;;; For two epsiodes e1 and e2, assert that
;;;; e1.st <= e2.end <= e2.st <= e2.end
;(defun ep-assert-precond-of (tg e1 e2)
;  (let* ((pair1 (tp-assert-before (gethash e1 (tg-str tg))
;							      (gethash e1 (tg-end tg))))
;		 (pair2 (tp-assert-before (second pair1)
;								  (gethash e2 (tg-str tg))))
;		 (pair3 (tp-assert-before (second pair2)
;								  (gethash e2 (tg-end tg))))
;		 (t1 (first pair1))
;		 (t2 (first pair2))
;		 (t3 (second pair2))
;		 (t4 (second pair3)))
;
;	(setf (gethash e1 (tg-str tg)) t1)
;	(setf (gethash e2 (tg-str tg)) t2)
;	(setf (gethash e1 (tg-end tg)) t3)
;	(setf (gethash e2 (tg-end tg)) t4)
;	(push e1 (tp-brefs t1))
;	(push e2 (tp-brefs t3))
;	(push e1 (tp-erefs t2))
;	(push e2 (tp-erefs t4))))
;
;
;; Querying functions
;; -------------------------------------------------------------------------
;
;(defun ep-consec-p (tg e1 e2)
;  (let ((e1str (gethash e1 (tg-str tg)))
;		(e1end (gethash e1 (tg-end tg)))
;		(e2str (gethash e2 (tg-str tg)))
;		(e2end (gethash e2 (tg-end tg))))
;
;	(and (tp-before-p e1str e1end)
;		 (tp-equals-p e1end e2str)
;		 (tp-before-p e2str e2end))))
;
;(defun ep-not-consec-p (tg e1 e2)
;  (let ((e1str (gethash e1 (tg-str tg)))
;		(e1end (gethash e1 (tg-end tg)))
;		(e2str (gethash e2 (tg-str tg)))
;		(e2end (gethash e2 (tg-end tg))))
;
;	(or (tp-not-before-p e1str e1end)
;		(tp-not-equals-p e1end e2str)
;		(tp-not-before-p e2str e2end))))
;
;(defun ep-equals-p (tg e1 e2)
;  (let ((e1str (gethash e1 (tg-str tg)))
;		(e1end (gethash e1 (tg-end tg)))
;		(e2str (gethash e2 (tg-str tg)))
;		(e2end (gethash e2 (tg-end tg))))
;
;	(and (tp-equals-p e1str e2str)
;		 (tp-equals-p e2end e2end))))
;
;(defun ep-not-equals-p (tg e1 e2)
;  (let ((e1str (gethash e1 (tg-str tg)))
;		(e1end (gethash e1 (tg-end tg)))
;		(e2str (gethash e2 (tg-str tg)))
;		(e2end (gethash e2 (tg-end tg))))
;
;	(or (tp-not-equals-p e1str e2str)
;		(tp-not-equals-p e1end e2end))))
;
;(defun ep-before-p (tg e1 e2)
;  (let ((e1str (gethash e1 (tg-str tg)))
;		(e1end (gethash e1 (tg-end tg)))
;		(e2str (gethash e2 (tg-str tg)))
;		(e2end (gethash e2 (tg-end tg))))
;
;	(and (tp-before-p e1str e2str)
;		 (tp-before-p e1str e1end)
;		 (tp-before-p e2str e2end))))
;
;(defun ep-not-before-p (tg e1 e2)
;  (let ((e1str (gethash e1 (tg-str tg)))
;		(e1end (gethash e1 (tg-end tg)))
;		(e2str (gethash e2 (tg-str tg)))
;		(e2end (gethash e2 (tg-end tg))))
;
;	(or (tp-not-before-p e1str e2str)
;		(tp-not-before-p e1str e1end)
;		(tp-not-before-p e2str e2end))))
;
;(defun ep-at-about-p (tg e1 e2)
;  (let ((e1str (gethash e1 (tg-str tg)))
;		(e1end (gethash e1 (tg-end tg)))
;		(e2str (gethash e2 (tg-str tg)))
;		(e2end (gethash e2 (tg-end tg))))
;
;	(and (tp-before-p e2str e1str)
;		 (tp-before-p e1str e1end)
;		 (tp-before-p e1end e2end))))
;
;(defun ep-not-at-about-p (tg e1 e2)
;  (let ((e1str (gethash e1 (tg-str tg)))
;		(e1end (gethash e1 (tg-end tg)))
;		(e2str (gethash e2 (tg-str tg)))
;		(e2end (gethash e2 (tg-end tg))))
;
;	(or (tp-not-before-p e2str e1str)
;		(tp-not-before-p e1str e1end)
;		(tp-not-before-p e1end e2end))))
;
;(defun ep-precond-of-p (tg e1 e2)
;  (let ((e1str (gethash e1 (tg-str tg)))
;		(e1end (gethash e1 (tg-end tg)))
;		(e2str (gethash e2 (tg-str tg)))
;		(e2end (gethash e2 (tg-end tg))))
;
;	(and (tp-before-p e1str e1end)
;		 (tp-before-p e1end e2str)
;		 (tp-before-p e2str e2end))))
;
;(defun ep-not-precond-of-p (tg e1 e2)
;  (let ((e1str (gethash e1 (tg-str tg)))
;		(e1end (gethash e1 (tg-end tg)))
;		(e2str (gethash e2 (tg-str tg)))
;		(e2end (gethash e2 (tg-end tg))))
;
;	(or (tp-not-before-p e1str e1end)
;		(tp-not-before-p e1end e2str)
;		(tp-not-before-p e2str e2end))))
;

; Quantitative Bounding functions
; -------------------------------------------------------------------------

;(defun set-lower-timebound (tg tp bound)
;  (setf (gethash tp (tg-lower tg)) bound))
;
;(defun set-upper-timebound (tg tp bound)
;  (setf (gethash tp (tg-upper tg)) bound))
;
;(defun set-lower-duration (tg t1 t2 bound) 
;  (setf (gethash (list t1 t2) (tg-lower tg)) bound))
;
;(defun set-upper-duration (tg t1 t2 bound) 
;  (setf (gethash (list t1 t2) (tg-upper tg)) bound))
;
;(defun get-lower-timebound (tg tp)
;  (gethash tp (tg-lower tg)))
;
;(defun get-upper-timebound (tg tp)
;  (gethash tp (tg-upper tg)))
;
;(defun get-lower-duration (tg t1 t2) 
;  (gethash (list t1 t2) (tg-lower tg)))
;
;(defun get-upper-duration (tg t1 t2) 
;  (gethash (list t1 t2) (tg-upper tg)))
;
;(defun insert-lower-bound (tg tp bound)
;  (let ((l1 (get-lower-timebound tg tp)))
;	(when (or (not l1) (< l1 bound))
;	  (set-lower-timebound tg tp bound)
;	  (dolist (tk (get-successors tp))
;		(let ((l2 (get-lower-timebound tg tk))
;			  (l (get-lower-duration tg tp tk)))
;		  (if (or (not l2) (< l2 (+ bound (fixnil l))))
;			(insert-lower-bound tg tk (+ bound (fixnil l))))
;		  (refresh-optimal-duration tg tp tk))))))
;
;(defun insert-upper-bound (tg tp bound)
;  (let ((u2 (get-upper-timebound tg tp)))
;	(when (or (not u2) (> u2 bound))
;	  (set-upper-timebound tg tp bound)
;	  (dolist (tk (get-ancestors tp))
;		(let* ((u1 (get-upper-timebound tg tk))
;			   (u (get-upper-duration tg tp tk)))
;		  (if (or (not u1) (> u1 (- bound (fixnil u))))
;			(insert-upper-bound tg tk (- bound (fixnil u))))
;		  (refresh-optimal-duration tg tk tp))))))
;
;(defun insert-lower-duration (tg t1 t2 bound)
;  (let ((l1 (get-lower-timebound tg t1))
;		(u1 (get-upper-timebound tg t1))
;		(l2 (get-lower-timebound tg t2))
;		(u2 (get-upper-timebound tg t2))
;		(l (get-lower-duration tg t1 t2)))
;
;	(when (or (not l) (< l bound))
;	  (if (and (not l1) (or (not l2) (< l2 (+ bound l1)))) 
;		(insert-lower-bound tg t2 (+ bound l1)))
;	  (if (and (not u2) (or (not u1) (> u1 (- u2 bound))))
;		(insert-upper-bound tg t1 (- u2 bound))))))
;
;(defun insert-upper-duration (tg t1 t2 bound)
;  (let ((l1 (get-lower-timebound tg t1))
;		(u1 (get-upper-timebound tg t1))
;		(l2 (get-lower-timebound tg t2))
;		(u2 (get-upper-timebound tg t2))
;		(u (get-upper-duration tg t1 t2)))
;
;	(when (or (not u) (> u bound))
;	  (if (and (not u1) (or (not u2) (> u2 (+ bound u1)))) 
;		(insert-upper-bound tg t2 (+ bound u1)))
;	  (if (and (not l2) (or (not l1) (< l1 (- l2 bound))))
;		(insert-lower-bound tg t1 (- l2 bound))))))
;  
;
;(defun refresh-optimal-duration (tg t1 t2)
;  (let ((l1 (get-lower-timebound tg t1))
;		(u1 (get-upper-timebound tg t1))
;		(l2 (get-lower-timebound tg t2))
;		(u2 (get-upper-timebound tg t2))
;		(l (get-lower-duration tg t1 t2))
;		(u (get-upper-duration tg t1 t2)))
;
;	(if (or (not l) (and l2 u1 (< l (- l2 u1))))
;	  (set-lower-duration tg t1 t2 (- l2 u1)))
;	(if (or (not u) (and u2 l1 (> u (- u2 l1))))
;	  (set-upper-duration tg t1 t2 (- u2 l1)))))
