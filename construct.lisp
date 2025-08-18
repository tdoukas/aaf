(in-package :aaf)

;;;
;;;  Utils
;;;

(defparameter *my-gensym-counter* 0)

(defun make-numbered-arg (&rest nums)
  (intern (format nil "Z~{~a~}" nums)))

(defun my-gensym ()
  (make-numbered-arg (incf *my-gensym-counter*)))

;;;
;;;  Classical Framework Construction
;;;

(defun construct-canonical-af (es)
  (let ((pairs (mapcan #'%set-pairs es))
	(args (union* es)))
    (make-aaf args
	      (remove-if (lambda (p) (member p pairs :test #'similar-pairs-p))
			 (%set-pairs args)))))

(defun %set-pairs (e)
  (let (pairs)
    (dolist (a e)
      (dolist (b e)
	(unless (equal a b)
	  (push (list a b) pairs))))
    pairs))

(defun construct-stable-af (es &aux (*my-gensym-counter* 0))
  (let* ((cf (construct-canonical-af es))
	 (stbs (stable-extensions cf))
	 (xss (remove-if (lambda (e) (have-set e es)) stbs))
	 (args (aaf-arguments cf))
	 (attacks (aaf-attacks cf)))
    (dolist (xs xss)
      (let ((z (my-gensym)))
	(dolist (a args)
	  (unless (member a xs)
	    (push (list a z) attacks)))
	(push (list z z) attacks)
	(push z args)))
    (make-aaf args attacks)))

(defun construct-defense-af (es &aux (*my-gensym-counter* 0))
  (let* ((canonical-aaf (construct-canonical-af es))
	 (args (aaf-arguments canonical-aaf))
	 (cnf-map (mapcar (lambda (arg)
			    (let ((defending-sets (mapcar (lambda (s) (remove arg s))
							  (remove-if-not (lambda (s) (member arg s)) es))))
			      (list arg (if (member nil defending-sets) nil (%all-tuples defending-sets)))))
			  args))
	 (attacks (aaf-attacks canonical-aaf)))
    (dolist (entry cnf-map)
      (destructuring-bind (arg cnf) entry
	(dolist (clause cnf)
	  (let ((z (my-gensym)))
	    (push z args)
	    (push (list z z) attacks)
	    (push (list z arg) attacks)
	    (dolist (a clause)
	      (push (list a z) attacks))))))
    (make-aaf args attacks)))

(defun %all-tuples (ss)
  (let (rs)
    (loop for s in ss when s do
      (if (null rs) (setq rs (mapcar #'list s))
	  (setq rs (loop for a in s append
			 (loop for b in rs collect (cons a b))))))
    (remove-duplicates (mapcar #'remove-duplicates rs) :test #'set-equal)))

;;;
;;;  Base Frameworks
;;;

(defun construct-ud-simple-base-framework (args)
  (let ((z 'z))
    (make-aaf (list* z args) (append (mapcar (lambda (a) (list z a)) args)
				     (list (list z z))))))

(defun construct-ud-base-framework (args)
  (make-aaf args (append (mapcan (lambda (a &aux (z (make-numbered-arg a)))
				 (list (list z z)
				       (list z a)))
			       args))))

(defun construct-ud-loop-base-framework (args)
  (augment-framework (construct-loop-af args) () '((x z0) (x x))))

(defun construct-wadm-simple-base-framework (args)
  (make-aaf args ()))

;;;
;;;  Loop AAF
;;;

(defun construct-loop-af (args &aux (*my-gensym-counter* 0))
  (let* ((z 'z0)
	 (aaf-args (list* z args))
	 (aaf-attacks ()))
    (dolist (a args)
      (let ((z2 (my-gensym)))
	(push z2 aaf-args)
	(push (list z a) aaf-attacks)
	(push (list a z2) aaf-attacks)
	(push (list z2 z) aaf-attacks)))
    (make-aaf aaf-args aaf-attacks)))

;;;
;;;  UD
;;;

(defun analyze-es (es)
  (let ((args (union* es)))
    (labels ((non-conflicting-args-pair (pair)
	       (member pair es :test #'subsetp)))
      (let* ((conflicting-args (remove-if #'non-conflicting-args-pair (all-pairs args)))
	     (non-conflicting-args (remove-if-not #'non-conflicting-args-pair (all-pairs args)))
	     (separated-exts (remove-if (lambda (pair)
					  (member (apply #'union pair) es :test #'set-equal))
					(all-pairs es)))
	     (separated-exts-not-ed (remove-if (lambda (ext-pair)
						 (destructuring-bind (x y) ext-pair
						   (some (lambda (arg-pair)
							   (destructuring-bind (a b) arg-pair
							     (or (and (member a x) (member b y))
								 (and (member a y) (member b x)))))
							 conflicting-args)))
					       separated-exts)))
	(list :conflicting-args conflicting-args
	      :non-conflicting-args non-conflicting-args
	      :separated-exts separated-exts
	      :separated-exts-not-ed separated-exts-not-ed)))))

;;;
;;;  Generic Uniquely Indexed
;;;

(defun construct-generic-af (es &aux (*my-gensym-counter* 0))
  (let (new-args new-attacks)
      (labels ((make-arg () (let ((arg (my-gensym)))
			      (push arg new-args)
			      arg))
	       (make-attack (a b)
		 (push (list a b) new-attacks)))
	(let* ((uis (unique-indices es))
	       (indices (mapcar #'second uis))
	       (other-args (set-difference (union* es) indices))
	       z1s)
	  (assert (notany #'null indices) () "Not uniquely indexed: ~s" es)
	  (loop for (a b) in (all-pairs indices) do
	    (make-attack a b)
	    (make-attack b a))
	  (dolist (arg other-args)
	    (let ((z1 (make-arg))
		  (z2 (make-arg)))
	      (make-attack z1 arg)
	      (make-attack arg z2)
	      (make-attack z2 z1)
	      (push (list arg z1) z1s)))
	  (dolist (e es)
	    (let* ((index (second (find e uis :key #'first)))
		   (others (remove index e)))
	      (dolist (arg others)
		(make-attack index (second (find arg z1s :key #'first))))))
	  ))	    
    (make-aaf new-args new-attacks)))

;;;
;;;  Framework Construction Macro
;;;

(defclass constr ()
  ((args :accessor constr-args :initarg :args)))

(defgeneric construct-method (constr &key))

(defun construct (constr &rest args)
  (let ((plist (mapcan (lambda (arg) (list arg t)) args)))
    (apply #'construct-method (if (symbolp constr) (funcall #'make-instance constr) constr) plist)))

(defun construct-from-bitmap (constr bitmap)
  (apply #'construct constr (decode-bitmap constr bitmap)))

(defun decode-bitmap (constr bitmap)
  (loop with args = (constr-args constr)
	for j from 0 below (length args)
	when (logbitp j bitmap) collect (intern (string (nth j args)) :keyword)))

(defmacro define-construction (name (&rest args) &body body)
  `(progn (defclass ,name (constr)
	    ())
	  (defmethod initialize-instance :after ((constr ,name) &key)
	    (setf (constr-args constr) ',args))
	  (defmethod construct-method ((constr ,name) &key ,@args)
	    ,@body)))

(defclass test-expressivity (rc)
  ((constr :accessor te-constr :initarg :constr)
   (bad :accessor te-bad :initform ())))

(defmethod initialize-instance :after ((rc test-expressivity) &key)
  (setf (rc-start rc) 0
	(rc-end rc) (expt 2 (length (constr-args (te-constr rc))))))

(defmethod rc-loop-body ((rc test-expressivity) (chunk rc-chunk) bitmap)
  (let* ((aaf (construct-from-bitmap (te-constr rc) bitmap))
	 (ud (undisputed-extensions aaf)))
    (if (position #\Z (princ-to-string ud) :test #'equalp)
	(sb-thread:with-mutex (*mutex*) (push (decode-bitmap (te-constr rc) bitmap) (te-bad rc)))
	(sb-thread:with-mutex (*mutex*)
	  (unless (member ud (rc-result rc) :key #'first
					    :test (lambda (a b) (set-equal a b :test #'set-equal)))
	    (push (list ud (decode-bitmap (te-constr rc) bitmap)) (rc-result rc)))))))

(defun test-constr-expressivity (constr &rest args)
  (unless (typep constr 'constr)
    (setq constr (apply #'make-instance constr args)))
  (let* ((n (length (constr-args constr)))
	 (rc (make-instance 'test-expressivity :constr constr :num-threads (min 8 (truncate n 2))))
	 (results (run-rc rc)))
    (values results (length results) (length (te-bad rc)) (when (te-bad rc) (first (te-bad rc))))))
