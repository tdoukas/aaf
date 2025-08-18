(in-package :aaf)

(defclass find-any-filtering-framework (repeated-test)
  ((aaf :accessor test-aaf :initarg :aaf)
   (args :accessor test-args :initarg :args)
   (semantics :accessor test-semantics :initarg :semantics)
   (max-additional-args :accessor test-max-additional-args :initarg :max-additional-args :initform 3)
   (other-than :accessor test-other-than :initarg :other-than :initform nil)
   (initial-es :accessor test-initial-es)))

(defmethod initialize-instance :after ((test find-any-filtering-framework) &key)
  (setf (test-initial-es test) (funcall (test-semantics test) (test-aaf test))))

(defmethod generate-test-subject ((test find-any-filtering-framework))
  (let* ((aaf (test-aaf test))
	 (num-additional-args (if (zerop (test-max-additional-args test)) 0
				  (random (test-max-additional-args test))))
	 (additional-args (loop for i below num-additional-args for j from 1
				collect (loop for arg = (make-numbered-arg j)
					      while (member arg (aaf-arguments aaf))
					      do (incf j) finally (return arg))))
	 (all-args (union additional-args (aaf-arguments aaf)))
	 (additional-attacks
	   (generate-subset-from-base-set
	    (all-pairs-symmetrical all-args))))
    (make-aaf all-args (union additional-attacks (aaf-attacks aaf)))))

(defmethod test-body ((test find-any-filtering-framework) (aaf aaf))
  (let* ((es (funcall (test-semantics test) aaf))
	 (coversp (aaf-covers-p aaf (test-aaf test)))
	 (all-args-p (set-equal (union* es) (test-args test)))
	 (newp (null (find es (test-other-than test) :test #'isomorphic-extensions))))
    (make-instance 'aaf-result
		   :aaf aaf
		   :properties (list :es es :coversp coversp :all-args-p all-args-p :newp newp)
		   :notablep (and newp coversp all-args-p (not (es-equal es (test-initial-es test)))))))

;;;
;;;  Find a framework that is the output of a filter operation producing a particular extension-set
;;;

(defclass find-filtering-framework (repeated-test)
  ((aaf :accessor test-aaf :initarg :aaf)
   (es :accessor test-es :initarg :es)
   (semantics :accessor test-semantics :initarg :semantics)
   (max-additional-args :accessor test-max-additional-args :initarg :max-additional-args :initform 3)))

(defmethod generate-test-subject ((test find-filtering-framework))
  (let* ((aaf (test-aaf test))
	 (num-additional-args (if (zerop (test-max-additional-args test)) 0
				  (random (test-max-additional-args test))))
	 (additional-args (loop for i below num-additional-args for j from 1
				collect (loop for arg = (make-numbered-arg j)
					      while (member arg (aaf-arguments aaf))
					      do (incf j) finally (return arg))))
	 (all-args (union additional-args (aaf-arguments aaf)))
	 (additional-attacks
	   (generate-subset-from-base-set
	    (all-pairs-symmetrical all-args))))
    (make-aaf all-args (union additional-attacks (aaf-attacks aaf)))))

(defmethod test-body ((test find-filtering-framework) (aaf aaf))
  (let ((es (funcall (test-semantics test) aaf))
	(coversp (aaf-covers-p aaf (test-aaf test))))
    (make-instance 'aaf-result
		   :aaf aaf
		   :properties (list :es es :coversp coversp)
		   :notablep (and coversp
				  (set-equal es (test-es test) :test #'set-equal)))))

;;;
;;;  Find framework with particular extension-set
;;;

(defun find-framework-with-extensions (&key es ess semantics (num-threads 1) (num-repeats 1)
					 (min-args 1) (max-args 12) (max-atts 0.8))
  (run-test (make-instance 'find-aaf-with-extensions
			   :sets (if es (list es) ess)
			   :semantics semantics :num-threads num-threads :num-repeats num-repeats
			   :min-args min-args :max-args max-args :max-atts max-atts)))

(defclass find-aaf-with-extensions (repeated-test)
  ((sets :accessor find-aaf-with-extensions-sets :initarg :sets)
   (semantics :accessor find-aaf-with-extensions-semantics :initarg :semantics
	      :initform #'weakly-admissible-extensions)
   (min-args :accessor repeated-test-min-args :initarg :min-args :initform 1)
   (max-args :accessor repeated-test-max-args :initarg :max-args :initform 12)
   (max-atts :accessor repeated-test-max-atts :initarg :max-atts :initform 0.8)))

(defmethod generate-test-subject ((test find-aaf-with-extensions))
  (with-slots (min-args max-args max-atts) test
    (let ((da (- max-args min-args)))
      (generate-aaf (if (plusp da) (+ min-args (random da)) min-args)
		    :ar (random max-atts)))))

(defmethod test-body ((test find-aaf-with-extensions) (aaf aaf))
  (let ((ess (find-aaf-with-extensions-sets test))
	(es (canonicalize-es (funcall (find-aaf-with-extensions-semantics test) aaf))))
    (make-instance 'aaf-result
		   :aaf aaf
		   :properties (list :es es)
		   :notablep (some (lambda (e) (isomorphic-extensions e es)) ess))))

(defun canonicalize-es (es)
  (let ((argmap (mapcar #'list
			(remove-duplicates (apply #'append (sort (copy-list es) #'< :key #'length)))
			'(a b c d e f g h i j k l m n o p q r s u v w x y))))
    (sort (mapcar (lambda (e) (mapcar (lambda (a) (second (find a argmap :key #'first))) e)) es)
	  #'< :key #'length)))

(defmethod postprocess-result :around ((test find-aaf-with-extensions) (result aaf-result))
  (let ((aaf (result-aaf result)))
    (make-instance 'aaf-result
		   :aaf (beautify-aaf aaf :es (funcall (find-aaf-with-extensions-semantics test) aaf))
		   :properties (result-properties result))))

(defun beautify-aaf (aaf &key (es (weakly-admissible-extensions aaf)))
  (let* ((args (aaf-arguments aaf))
	 (eargs (union* es))
	 (a-y '(a b c d e f g h i j k l m n o p q r s t u v w x y))
	 argmap)
    (dolist (a eargs)
      (push (list a (pop a-y)) argmap))
    (loop for a in (set-difference args eargs) for n from 1 do
      (push (list a (intern (format nil "Z~a" n))) argmap))
    (canonicalize-aaf aaf :argmap (nreverse argmap))))

;;;
;;; Find frameworks with certain properties
;;;

(defclass fw-property ()
  ())

(defgeneric test-fw-property (fw-property aaf))

(defclass find-framework-with-properties (repeated-test)
  ((property :accessor fw-property :initarg :property)))

(defgeneric generate-test-subject-for-property (fw-property))

(defmethod generate-test-subject-for-property ((prop fw-property))
  (generate-aaf (random 12) :ar (random 0.6)))

(defmethod generate-test-subject ((test find-framework-with-properties))
  (generate-test-subject-for-property (fw-property test)))

(defmethod test-body ((test find-framework-with-properties) (aaf aaf))
  (multiple-value-bind (props notablep) (test-fw-property (fw-property test) aaf)
    (make-instance 'aaf-result :aaf aaf :properties props :notablep notablep)))

(defun find-framework-with-properties (property  &key (num-repeats 1000) (num-threads 1))
  (run-test (make-instance 'find-framework-with-properties
			   :property (if (symbolp property) (make-instance property) property)
			   :num-repeats num-repeats :num-threads num-threads)))

;;;
;;; Properties to test for
;;;

(defmacro define-fw-property (name parameter-list &body body)
  (let ((aaf (first parameter-list))
	(parameters (rest parameter-list)))
    `(progn (defclass ,name (fw-property)
	      ,(mapcar (lambda (p)
			 `(,p :initarg ,(intern (string p) :keyword)))
		parameters)
	      )
	    (defmethod test-fw-property ((prop ,name) (,aaf aaf))
	      (with-slots ,parameters prop
		,@body)))))
  
(define-fw-property different-adm-pref-compl-stab (aaf) 
  (let* ((adm (admissible-extensions aaf))
	 (pref (preferred-extensions aaf))
	 (compl (complete-extensions aaf))
	 (stab (stable-extensions aaf))
	 (notablep (not (or (set-equal adm pref)
			    (set-equal compl adm)
			    (set-equal pref compl)
			    (set-equal pref stab)
			    (set-equal compl stab)
			    (null stab)
			    (> (length (aaf-arguments aaf)) 4)
			    ))))
    (values (list :adm adm :pref pref :compl compl :stab stab)
	    notablep)))

(define-fw-property wadm-not-dcl (aaf)
  (let* ((wadm (weakly-admissible-extensions aaf))
	 (dclp (downward-closed-p wadm)))
    (values (list :wadm wadm :dclp dclp)
	    (not dclp))))

(define-fw-property wpr-ud-sud-same (aaf)
  (let* ((wpr (weakly-admissible-extensions aaf))
	 (ud (undisputed-extensions aaf))
	 (sud (strongly-undisputed-extensions aaf))
	 (samep (and (not (equalp '(())  wpr))
		     (isomorphic-extensions wpr ud)
		     (isomorphic-extensions wpr sud))))
    (values (list :wpr wpr :ud ud :sud sud)
	    samep)))

(define-fw-property canfw-not-ud-basefw (aaf)
  (let* ((ud (undisputed-extensions aaf))
	 (canfw (construct-canonical-af ud))
	 (es (undisputed-extensions canfw))
	 (basefwp (subsetp ud es :test #'set-equal)))
    (values (list :ud ud :canfw canfw :es es) (not basefwp))))

;;;
;;;  Find framework that has two weakly admissible extensions X,Y so that their union is not weakly
;;;  admissible, and there is no Z in the weakly admissible extensions of the reduct F^(X\cup Y)
;;;  that attacks both X and Y.
;;;

(defclass find-wadm-no-union (repeated-test)
  ())

(defmethod generate-test-subject ((test find-wadm-no-union))
  (generate-aaf (random 10) :ar (random 0.4)))

(defmethod test-body ((test find-wadm-no-union) (aaf aaf))
  (let* ((es (weakly-admissible-extensions aaf))
	 (cf-e-pairs
	   (remove-duplicates (remove-if (lambda (pair)
					   (not (conflict-free (apply #'union pair) (aaf-attacks aaf))))
					 (collect-extension-pairs-no-union es))
			      :test (lambda (n1 n2)
				      (set-equal n1 n2 :test #'set-equal))))
	 (setspecs (mapcar (lambda (pair)
			     (destructuring-bind (x y) pair
			       (let* ((x- (attackers-of aaf x))
				      (y- (attackers-of aaf y))
				      (x+ (attacked-from aaf x))
				      (y+ (attacked-from aaf y))
				      (reduct (reduct aaf (union x y)))
				      (reduct-wadm (weakly-admissible-extensions reduct))
				      (zs (remove-if-not (lambda (z)
							   (and (intersection z x-)
								(intersection z y-)))
							 reduct-wadm)))
				 (list :x x :y y :x- x- :y- y- :x+ x+ :y+ y+ :zs zs
				       :reduct-wadm reduct-wadm
				       ))))
			   cf-e-pairs))
	 (notablep (some (lambda (setspec)
			   (destructuring-bind (&key zs &allow-other-keys) setspec
			     (null zs)))
			 setspecs)))
    (make-instance 'aaf-result
		   :aaf aaf
		   :properties (list :cf-e-pairs cf-e-pairs :setspecs setspecs :notablep (when notablep t))
		   :notablep notablep)))

;;;
;;;  Find framework whose weakly admissible extension-set is not conflict-sensitive
;;;

(defclass find-wadm-not-cs (repeated-test)
  ())

(defmethod generate-test-subject ((test find-wadm-not-cs))
  (generate-aaf (+ 3 (random 7)) :ar (random 0.6)))

(defmethod test-body ((test find-wadm-not-cs) (aaf aaf))
  (let* ((es (weakly-admissible-extensions aaf))
	 (csp (conflict-sensitive-p es)))
    (make-instance 'aaf-result
		   :aaf aaf :properties (list :es es :csp csp)
		   :notablep (and (not csp)))))

;;;
;;; Find extension-set
;;;

(defclass find-extension-set (repeated-test)
  ())
  
(defmethod generate-test-subject ((test find-extension-set))
  (generate-extension-set (random 4) (random 4)))

(defclass disj-pairing-and-disj-supp (find-extension-set)
  ())

(defmethod test-body ((test disj-pairing-and-disj-supp) (es list))
  (let ((dpp (disjoint-pairing-p es))
	(dsp (disjointly-supported-p es)))
    (make-instance 'extension-set-result
		   :es es :properties (list :dpp dpp :dsp dsp)
		   :notablep (and dpp dsp))))

(defclass confl-sens-but-not-tight (find-extension-set)
  ())

(defmethod test-body ((test confl-sens-but-not-tight) (es list))
  (make-instance 'extension-set-result
		 :es es :notablep (and (conflict-sensitive-p es)
				       (not (tightp es)))))

(defclass pref-exts-not-in-canonical-framework (find-extension-set)
  ())

(defmethod generate-test-subject ((test pref-exts-not-in-canonical-framework))
  (generate-extension-set 5 3))

(defmethod test-body ((test pref-exts-not-in-canonical-framework) (es list))
  (if (and (not (null es))
	   (incomparablep es)
	   (conflict-sensitive-p es))
      (let* ((f (construct-canonical-af es))
	     (f-pref (preferred-extensions f))
	     (subsetp (subsetp es f-pref :test #'set-equal)))
	(make-instance 'extension-set-result
		       :es es :notablep (not subsetp)))
      (make-instance 'extension-set-result :es es :notablep nil)))

(defclass stb-exts-not-in-canonical-framework (find-extension-set)
  ())

(defmethod generate-test-subject ((test stb-exts-not-in-canonical-framework))
  (generate-extension-set (random 8) (random 8)))

(defmethod test-body ((test stb-exts-not-in-canonical-framework) (es list))
  (if (and (incomparablep es)
	   (tightp es))
      (let* ((f (construct-canonical-af es))
	     (f-stb (stable-extensions f))
	     (subsetp (subsetp es f-stb :test #'set-equal)))
	(make-instance 'extension-set-result
		       :es es :notablep (not subsetp)))
      (make-instance 'extension-set-result :es es :notablep nil)))
