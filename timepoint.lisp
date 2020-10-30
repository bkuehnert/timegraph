(in-package #:timegraph.implementation)

;;; * chain: Unique chain identifier. Two timepoints will have the same
;;;          chain value iff they are on the same chain.
;;; * prev: Link object corresponding to the previous link in the chain.
;;; * next: Link object corresponding to the next link in the chain.
;;; * ptime: Pseudotime of a timepoint.
;;; * inc: List of cross-chain links entering the timepoint.
;;; * out: List of cross-chain links exiting the timepoint.
;;; * brefs: List of episode names which the timepoint begins.
;;; * erefs: List of episode names which the timepoint ends.
(defclass timepoint ()
  ((chain :type integer
          :accessor tp-chain
          :initarg :chain
          :initform (sxhash (gensym)))
   (prev :type (or null link)
         :accessor tp-prev
         :initarg :prev
         :initform nil)
   (next :type (or null link)
         :accessor tp-next
         :initarg :next
         :initform nil)
   (ptime :type integer
          :initarg :ptime
          :accessor tp-ptime
          :initform 0)
   (inc :type list
        :initarg :inc
        :accessor tp-inc
        :initform ())
   (out :type list
        :initarg :out
        :accessor tp-out
        :initform ())
   (brefs :type list
          :initarg :brefs
          :accessor tp-brefs
          :initform ())
   (erefs :type list
          :initarg :erefs
          :accessor tp-erefs
          :initform ())))

(defun make-timepoint (&key (chain (sxhash (gensym))) prev next (ptime 0)
                         (inc ()) (out ()) (brefs ()) (erefs ()))
  (make-instance 'timepoint :chain chain :prev prev :next next :ptime ptime
                 :inc inc :out out :brefs brefs :erefs erefs))

;; Represents a link between two timepoints. In the future, this will store
;; absolute temporal bounds.
(defclass link ()
  ((src :type timepoint
        :accessor link-src
        :initarg :src)
   (dst :type timepoint
        :accessor link-dst
        :initarg :dst)))

(defun make-link (&key src dst)
  (make-instance 'link :src src :dst dst))

;;; Utility Functions
;;; -----------------------------------------------------------------------

;;; Get list of direct successors of a timepoint t1
(defun get-successors (t1)
  (when t1
    (let ((succ (mapcar #'link-dst (tp-out t1))))
      (if (tp-next t1)
          (cons (link-dst (tp-next t1)) succ)
          succ))))

;;; Get list of direct ancestors of a timepoint t1
(defun get-ancestors (t1)
  (when t1
    (let ((anc (mapcar #'link-src (tp-inc t1))))
      (if (tp-prev t1)
          (cons (link-src (tp-prev t1)) anc)
          anc))))

;;; Get list of all successors of a timepoint t1
(defun get-all-successors (t1)
  (let ((table (make-hash-table :test #'equal)))
    (labels ((dfs (tk)
               (unless (gethash tk table)
                 (setf (gethash tk table) t)
                 (cons tk (reduce #'append
                                  (mapcar #'dfs (get-successors tk)))))))
      (dfs t1))))

;;; Get list of all ancestors of a timepoint t1.
(defun get-all-ancestors (t1)
  (let ((table (make-hash-table :test #'equal)))
    (labels ((dfs (tk)
               (unless (gethash tk table)
                 (setf (gethash tk table) t)
                 (cons tk (reduce #'append
                                  (mapcar #'dfs (get-ancestors tk)))))))
      (dfs t1))))

;;; Check if a timepoint t1 is the last in its chain.
(defun last-p (t1)
  (not (tp-next t1)))

;;; Check if a timepoint t1 is the first in its chain.
(defun first-p (t1)
  (not (tp-prev t1)))

;; Recursively update a chain's pseudotime and chain hash to
;; current + PTIME, and CHAIN respectively. HEAD is a timepoint
;; which begins the chain in question.
(defun update-chain (head chain ptime)
  (setf (tp-chain head) chain)
  (setf (tp-ptime head) (+ (tp-ptime head) ptime))
  (when (tp-next head)
    (update-chain (link-dst (tp-next head)) chain ptime)))

;;; For two existing timepoints t1 and t2, establish a cross-chain link t1 -> t2.
(defun add-cross-link (t1 t2)
  (let ((newlink (make-link :src t1 :dst t2)))
    (unless (or (or (not t1) (not t2))
                (some (lambda (link) (eq t2 (link-dst link))) (tp-out t1))
                (and (tp-next t1) (equal (link-dst (tp-next t1)) t2)))
      (push newlink (tp-out t1))
      (push newlink (tp-inc t2)))))

;;; Insertion methods
;;; -----------------------------------------------------------------------

;;; Given a timepoint t1, creates and returns a new timepoint which is
;;; directly after t1 in the graph.
(defun insert-timepoint-after (t1 &key brefs erefs)
  (cond 
    ((not t1) nil) ;;; add error message here
    ((last-p t1)
     (let* ((ret (make-timepoint
                  :chain (tp-chain t1)
                  :ptime (1+ (tp-ptime t1))
                ;;  :lower (tp-lower t1)
                  :brefs brefs
                  :erefs erefs))
            (newlink (make-link :src t1 :dst ret)))
       (setf (tp-prev ret) newlink)
       (setf (tp-next t1) newlink)
       ret))
    (t
     (let ((ret (make-timepoint :brefs brefs :erefs erefs)))
       (add-cross-link t1 ret)
       ret))))

;;; Given a timepoint t1, creates and returns a new timepoint which is
;;; directly before t1 in the graph.
(defun insert-timepoint-before (t1 &key brefs erefs)
  (let (ret)
    (cond 
      ((not t1) nil) ; add error
      ((first-p t1)
       (setf ret (make-timepoint
                  :chain (tp-chain t1)
                  :ptime (1- (tp-ptime t1))
                 ; :upper (tp-upper t1)
                  :brefs brefs
                  :erefs erefs))
       (let ((newlink (make-link :src ret :dst t1)))
         (setf (tp-next ret) newlink)
         (setf (tp-prev t1) newlink))
       ret)
      (t
       (setf ret (make-timepoint :brefs brefs :erefs erefs))
       (add-cross-link ret t1)
       ret))))

;;; t1 and t2 are timepoints (either are possibly nil), assert that t1 is
;;; before t2 and return them. Preconditions: If t1 and t2 are not nil,
;;; then it must be the case that t2 is not before t1, otherwise, the
;;; timegraph will be in a contradictory state after running this
;;; function.
(defun tp-assert-before (tg t1 t2)
  (cond
    ((and (not t1) (not t2))
     (let* ((t1 (make-timepoint))
            (t2 (insert-timepoint-after t1)))
       (list t1 t2)))
    ((not t1)
     (list (insert-timepoint-before t2) t2))
    ((not t2)
     (list t1 (insert-timepoint-after t1)))
    ((tp-before-p t1 t2)
     (list t1 t2))
    ((tp-before-p t2 t1)
     (tp-assert-equal tg t2 t1))
    ((and (last-p t1) (first-p t2))
     (let ((new-link (make-link :src t1 :dst t2)))
       (setf (tp-next t1) new-link)
       (setf (tp-prev t2) new-link)
       (update-chain t2 (tp-chain t1) (tp-ptime t1))
       (list t1 t2)))
    (t
     (add-cross-link t1 t2)
     (list t1 t2))))

;;; t1 and t2 are timepoints (either are possibly nil), assert that t1 is
;;; equal to t2 and return (t1 t2). Preconditions: none. Function also
;;; requires reference to a timegraph tg, since updates to the timegraph's
;;; references are needed in some cases.
(defun tp-assert-equal (tg t1 t2)
  (cond 
    ((tp-equal-p t1 t2)
     (list t1 t2))

    ((and (not t1) (not t2))
     (let ((t1 (make-timepoint)))
       (list t1 t1)))

    ((not t1)
     (list t2 t2))

    ((not t2)
     (list t1 t1))

    ((tp-before-p t1 t2)
     (tp-assert-equal-helper tg t1 t2)
     (list t1 t1))

    ((tp-before-p t2 t1)
     (tp-assert-equal-helper tg t2 t1)
     (list t2 t2))

    (t
     (tp-assert-before nil t1 t2)
     (tp-assert-equal-helper tg t1 t2)
     (list t1 t1))))

;; In the case that t1 and t2 exist and t2 is after t1, then in order
;; to assert that t1 = t2, all timepoints between t2 and t1 must be
;; set equal to eachother (and thus equal to t1). This helper function
;; searches for such points and updates their references with a given
;; timegraph object tg.
;; TODO: re-implement this recursively.
(defun tp-assert-equal-helper (tg t1 t2)
  (let* ((t1suc (get-all-successors t1))
         (t2anc (get-all-ancestors t2))
         (quo (remove t1 (intersection t1suc t2anc))))
    ;; Note: There's a possible optimization to make when t1.next either does not exist,
    ;; or is in quo. In this case, we want to add some out-edge of t2 as the link in t1's
    ;; chain. This shouldn't be hard to do, but the code is already complex enough.

    ;;move through timepoints between t1 and t2 and redirect all of their edges through
    ;;to t1
    (dolist (tk quo)
      ;; Update incoming chain
      (when (tp-prev tk)
        (let ((tk-prev (link-src (tp-prev tk))))
          (unless (member tk-prev quo)
            (add-cross-link tk-prev t1))
          (setf (tp-prev tk) nil)
          (setf (tp-next tk-prev) nil)))

      (when (tp-next tk)
        (let ((tk-next (link-dst (tp-next tk))))
          (unless (member tk-next quo)
            (add-cross-link t1 tk-next)
            (update-chain tk-next (sxhash (gensym)) 0))
          (setf (tp-next tk) nil)
          (setf (tp-prev tk-next) nil)))

      (dolist (in-link (tp-inc tk))
        (setf (tp-out (link-src in-link))
              (delete-if (lambda (link) (equal (link-dst link) tk))
                         (tp-out (link-src in-link))))
        (add-cross-link (link-src in-link) t1))
      (setf (tp-inc tk) nil)

      (dolist (out-link (tp-out tk))
        (setf (tp-inc (link-dst out-link))
              (delete-if (lambda (link) (equal (link-src link) tk))
                         (tp-inc (link-dst out-link))))
        (add-cross-link t1 (link-dst out-link)))
      (setf (tp-out tk) nil)

      ;; Update the beginning and end refs of t1 to include all deleted timepoints.
      (setf (tp-brefs t1) (union (tp-brefs t1) (tp-brefs tk)))
      (setf (tp-erefs t1) (union (tp-erefs t1) (tp-erefs tk)))

      ;; Update the timegraph to reflect this. After this, all timepoints in quo should
      ;; be dangling, and garbage collected.
      (dolist (bref (tp-brefs tk))
        (set-beg tg bref t1))
      (dolist (eref (tp-erefs tk))
        (set-end tg eref t1)))

    ;; Clean up orphaned links. Any link from quo -> quo is now a self-loop on t1. Those
    ;; should be removed from the list of links and garbage collected.
    (setf (tp-inc t1) (delete-if (lambda (link) (equal (link-src link) (link-dst link))) (tp-inc t1)))
    (setf (tp-out t1) (delete-if (lambda (link) (equal (link-src link) (link-dst link))) (tp-out t1)))))

;;; Querying functions
;;; ----------------------------------------------------------------------

;;; Returns t if t1 is before or equal to t2. Returns nil if t1 is after
;;; t2 or there is no relation found.
;;; todo: add shortcut if quant bounds allow it (with optional tg reference)
(defun tp-before-p (t1 t2)
  (if (or (not t1) (not t2))
      nil
      (let ((seen (make-hash-table :test #'equal)))
        (labels
            ((dfs (tk)
               (cond
                 ((and (equal (tp-chain tk) (tp-chain t2)))
                  (<= (tp-ptime tk) (tp-ptime t2)))
                 ((not (gethash tk seen))
                  (setf (gethash tk seen) t)
                  (dolist (next (get-successors tk))
                    (when (dfs next)
                      (return-from tp-before-p t)))))))
          (dfs t1)))))

;;; Returns t if t1 is equal to t2. Returns nil if the inference cannot be
;;; made.
(defun tp-equal-p (t1 t2)
  (equal t1 t2))

