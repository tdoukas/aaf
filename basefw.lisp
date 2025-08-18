(in-package :aaf)

;;;
;;; BASEFW
;;;

(defclass basefw ()
  ((aaf :accessor basefw-aaf :initarg :aaf)
   (args :accessor basefw-args :initarg :args)
   (semantics :accessor basefw-semantics :initarg :semantics)
   (es :accessor basefw-es)))

(defmethod initialize-instance :after ((basefw basefw) &key)
  (setf (basefw-es basefw) (sort-extension-set (funcall (basefw-semantics basefw) (basefw-aaf basefw)))))

(defmethod print-object ((basefw basefw) out)
  (print-unreadable-object (basefw out :type t)
    (format out "~s ~s" (basefw-semantics basefw) (basefw-es basefw))))

(defun make-basefw (semantics constructor args)
  (make-instance 'basefw :aaf (funcall constructor args) :args args :semantics semantics))

(defun make-ud-basefw (args &key (constructor 'construct-ud-base-framework))
  (make-basefw 'undisputed-extensions constructor args))

(defun make-wadm-basefw (args &key (constructor 'construct-wadm-simple-base-framework))
  (make-basefw 'weakly-admissible-extensions constructor args))

;;;
;;; FILTER-RESULT
;;;

(defclass filter-result ()
  ((basefw :accessor fr-basefw :initarg :basefw)
   (aaf :accessor fr-aaf :initarg :aaf)
   (es :accessor fr-es :initarg :es)
   (diff :accessor fr-diff :initarg :diff)
   (attacks :accessor fr-attacks :initarg :attacks)))

(defmethod print-object ((fr filter-result) out)
  (print-unreadable-object (fr out :type t)
    (format out "removed=~s remaining=~s~%  attacks=~s" (fr-diff fr) (fr-es fr) (fr-attacks fr))))

;;;
;;; Attack Patterns
;;;

(defun make-attack-patterns (args ignore)
  (let ((n (length args))
	attacks)
    (dotimes (row n)
      (dotimes (col n)
	(let ((attack (list (nth row args) (nth col args))))
	  (unless (member attack ignore :test #'equal)
	    (push attack attacks)))))
    (nreverse attacks)))

(defun make-attack-patterns-for-aaf (aaf)
  (make-attack-patterns (aaf-arguments aaf)
			(union (aaf-attacks aaf)
			       (mapcar (lambda (arg) (list arg arg)) (aaf-arguments aaf)))))

(defun generate-attacks (patterns bitmap)
  (loop for i from 0 for attack in patterns
	when (logbitp i bitmap) collect attack))

;;;
;;; Apply Filter
;;;

(defun apply-covering-filter (basefw args attacks)
  (let* ((aaf (augment-framework (basefw-aaf basefw) args attacks))
	 (es (funcall (basefw-semantics basefw) aaf))
	 (diff (set-difference (basefw-es basefw) es :test #'set-equal)))
    (make-instance 'filter-result :basefw basefw :aaf aaf :es es :diff diff :attacks attacks)))
  
(defun generate-and-apply-filter (basefw bitmap
				  &key (patterns (make-attack-patterns-for-aaf (basefw-aaf basefw))))
  (let ((attacks (generate-attacks patterns bitmap)))
    (apply-covering-filter basefw () attacks)))

;;;
;;; FIND-FILTER-ATTACKS
;;;

(defclass find-filter-attacks (rc)
  ((basefw :accessor ffa-basefw :initarg :basefw)
   (patterns :accessor ffa-patterns)))

(defmethod initialize-instance :after ((rc find-filter-attacks) &key)
  (setf (ffa-patterns rc) (make-attack-patterns-for-aaf (basefw-aaf (ffa-basefw rc)))
	(rc-start rc) 0
	(rc-end rc) (expt 2 (length (ffa-patterns rc)))))

(defclass ffa-result ()
  ((removed :accessor rcr-removed :initarg :removed)
   (filter-result :accessor rcr-filter-result :initarg :filter-result)
   (count :accessor rcr-count :initarg :count :initform 0)))

(defmethod print-object ((rcr ffa-result) out)
  (print-unreadable-object (rcr out :type t)
    (format out "removed=~s count=~s" (rcr-removed rcr) (rcr-count rcr))))

(defmethod rc-loop-body ((rc find-filter-attacks) (chunk rc-chunk) bitmap)
  (let* ((result (generate-and-apply-filter (ffa-basefw rc) bitmap))
	 (removed (fr-diff result)))
    (when removed
      (sb-thread:with-mutex (*mutex*)
	(let ((rcr (or (find removed (rc-result rc)
			     :key #'rcr-removed :test (lambda (a b) (set-equal a b :test #'set-equal)))
		       (let ((rcr (make-instance 'ffa-result :removed removed :filter-result result)))
			 (push rcr (rc-result rc))
			 rcr))))
	  (incf (rcr-count rcr)))))
    (rc-periodically rc (lambda () (logmsg "results: ~s (~a)" (length (rc-result rc))
					   (loop for rcr in (rc-result rc) summing (rcr-count rcr)))))))

