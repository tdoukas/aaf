(in-package :aaf)

(defgeneric all-p/r-es (semantics))

(defmethod all-p/r-es ((semantics (eql 'weakly-admissible-extensions)))
  (remove-if-not (lambda (es) (member nil es)) (power-set (power-set '(a b c)))))

(defmethod all-p/r-es ((semantics (eql 'weakly-preferred-extensions)))
  (remove-if-not (lambda (es) (and es (incomparablep es))) (power-set (power-set '(a b c)))))

(defmethod all-p/r-es ((semantics (eql 'undisputed-extensions)))
  (remove-if
   (lambda (e) (or (disjointly-supported-p e)
		   (disjoint-pairing-p e)))
   (power-set (power-set '(a b c)))))

(defmethod all-p/r-es ((semantics (eql 'strongly-undisputed-extensions)))
  (remove-if-not #'incomparablep (power-set (power-set '(a b c)))))

(defun all-three-arg-p/r-es (semantics)
  (remove-if-not #'three-arg-p (all-p/r-es semantics)))

(defun all-p/r-es-types (semantics)
  (sort 
   (remove-duplicates (all-three-arg-p/r-es semantics) :test #'isomorphic-extensions)
   #'< :key #'length))

(defvar *realization-store-mutex* (sb-thread:make-mutex))

(defun all-realizations (semantics)
  (sb-thread:with-recursive-lock (*realization-store-mutex*)
    (getf *realizations* semantics)))

(defun store-realization (semantics es-type form)
  (sb-thread:with-recursive-lock (*realization-store-mutex*)
    (let ((rs (all-realizations semantics)))
      (let ((r (find es-type rs :test #'isomorphic-extensions :key #'first)))
	(if r (setf (second r) form)
	    (setq rs (append rs (list (list es-type form))))))
      (setf (getf *realizations* semantics) rs))))

(defun store-realizations-to-file ()
  (sb-thread:with-recursive-lock (*realization-store-mutex*)
    (with-open-file (out "realizations.lisp" :direction :output :if-exists :supersede)
      (prin1 '(in-package :aaf) out)
      (terpri out)
      (prin1 `(defvar *realizations* ',*realizations*) out))))
      
(defun get-realization (semantics es-type)
  (sb-thread:with-recursive-lock (*realization-store-mutex*)
    (second (find es-type (all-realizations semantics) :test #'isomorphic-extensions :key #'first))))      

(defun all-realized-es-types (semantics)
  (mapcar #'first (all-realizations semantics)))

(defun all-unrealized-es-types (semantics)
  (set-difference (all-p/r-es-types semantics) (all-realized-es-types semantics)
		  :test #'isomorphic-extensions))

(defun find-realization (semantics &key (num-threads 1) (num-repeats 100000) (min-args 1) (max-args 12)
				     (max-atts 0.8))
  (let* ((wanted (all-unrealized-es-types semantics))
	 (result (find-framework-with-extensions
		  :ess wanted :semantics semantics :num-threads num-threads
		  :num-repeats num-repeats :min-args min-args :max-args max-args :max-atts max-atts)))
    (when result
      (let* ((aaf (result-aaf result))
	     (es (funcall semantics aaf)))
	(store-realization semantics es (aaf-form aaf))
	(store-realizations-to-file)
	(format t " **** FOUND REALIZATION FOR ~s: ~s~%" es aaf)
	es))))

(defun keep-finding-realizations (semantics-list &key (num-threads 8) (num-repeats 100000)
						   (min-args 1) (max-args 12)
						   (max-atts 0.8))
  (loop for n from 0
	for semantics = (nth (mod n (length semantics-list)) semantics-list)
	for num-found = (length (all-realized-es-types semantics))
	for num-missing = (length (all-unrealized-es-types semantics))
	while (plusp num-missing) do
	  (format t " ***********************************~%")
	  (format t " ** ~s~%" semantics)
	  (format t " ** FOUND: ~s, TO GO: ~s~%" num-found num-missing)
	  (format t " ***********************************~%")
	  (find-realization semantics :num-threads num-threads :num-repeats num-repeats
				      :min-args min-args :max-args max-args :max-atts max-atts)))
    
(defun report-unrealized-es-types (&optional (semantics-list '(weakly-admissible-extensions weakly-preferred-extensions undisputed-extensions strongly-undisputed-extensions)))
  (dolist (semantics semantics-list)
    (format t "~s~%" semantics)
    (format t "  Found: ~s, Missing: ~s~%" (length (all-realized-es-types semantics))
	    (length (all-unrealized-es-types semantics)))))
  
    
