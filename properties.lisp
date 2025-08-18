(in-package :aaf)

;;
;;  Properties of Extensions
;;

(defun find-elements-in-extensions (s es)
  "Find a set of ES that contains S."
  (block fn
    (dolist (e es)
      (when (every (lambda (a) (member a e)) s)
	(return-from fn e)))
    nil))

(defun incomparablep (es)
  "Tell whether the sets of E are pairwise incomparable."
  (block fn
    (loop for s1 in es do
      (loop for s2 in es do
	(when (not (eq s1 s2))
	  (if (and (subsetp s1 s2)
		   (not (set-equal s1 s2)))
	      (return-from fn (values nil (list s1 s2)))))))
    t))

(defun tightp (es)
  "Tell whether ES is tight."
  (block fn
    (loop for a in (union* es) do
      (loop for s in es do
	(let ((s2 (union (list a) s)))
	  (when (not (find s2 es :test #'set-equal))
	    (when (every (lambda (b)
			   (find-elements-in-extensions (list a b) es))
			 s)
	      (return-from fn (values nil s2)))))))
    t))

(defun conflict-sensitive-p (es)
  "Tell whether ES is conflict-sensitive."
  (block fn
    (loop for s1 in es do
	 (loop for s2 in es do
	   (let ((union (union s1 s2)))
	     (when (and (not (have-set union es))
			(every (lambda (a)
				 (every (lambda (b)
					  (find-elements-in-extensions
					   (list a b) es))
					union))
			       union))
	       (return-from fn (values nil union))))))
    t))

(defun completion-sets (es s)
  (let ((all-containing (remove-if-not (lambda (e) (subsetp s e)) es)))
    (when all-containing
      (let ((smallest-length (apply #'min (mapcar #'length all-containing))))
	(remove-if-not (lambda (e) (= (length e) smallest-length)) all-containing)))))

(defun com-closed-p (es)
  "Tell whether ES is com-closed."
  (block fn
    (loop for s1 in es do
	 (loop for s2 in es do
	   (let ((union (union s1 s2)))
	     (when (and (not (= 1 (length (completion-sets es union))))
			(every (lambda (a)
				 (every (lambda (b)
					  (find-elements-in-extensions
					   (list a b) es))
					union))
			       union))
	       (return-from fn (values nil union))))))
    t))

(defun downward-closed-p (es)
  "Tell whether ES is downward-closed."
  (block fn
    (dolist (e es)
      (enumerate-subsets e (lambda (s)
			     (unless (member s es :test #'set-equal)
			       (return-from fn (values nil s))))))
    t))

(defun collect-extension-pairs-no-union (es)
  "Collect pairs of extension whose union is not present in ES."
  (let (res)
    (loop for s1 in es do
      (loop for s2 in es do
	(let ((union (union s1 s2)))
	  (when (and (not (have-set union es))
		     (push (list s1 s2) res))))))
    (nreverse res)))
  
(defun contains-intersection-p (es)
  (member (reduce #'intersection es) es :test #'set-equal))

(defun evidently-in-conflict (xs ys es)
  (block fn
    (dolist (x xs)
      (dolist (y ys)
	(let ((xy (list x y)))
	  (when (notany (lambda (s)
			  (subsetp xy s))
		      es)
	  (return-from fn xy)))))))

(defun evident-conflict-p (es)
  (some (lambda (pair)
	  (destructuring-bind (xs ys) pair
	    (evidently-in-conflict xs ys es)))
	(all-pairs es)))

(defun disjointly-supported-p (es)
  (and (> (length es) 1)
       (every #'identity es)
       (null (reduce #'intersection es))
       (not (evident-conflict-p es))))

(defun disjoint-pairing-p (es)
  (and (not (member nil es))
       (every (lambda (e)
		(some (lambda (e2)
			(and (set-difference e e2)
			     (member (union e e2) es :test #'set-equal)))
		      es))
	      es)))

(defun unique-indices (es)
  (mapcar (lambda (e &aux (complement (set-difference es (list e))))
	    (list e (first (remove-if-not
			    (lambda (a)
			      (notany (lambda (e2)
					(member a e2))
				      complement))
			    e))))
	  es))

(defun uniquely-indexed-p (es)
  (every #'second (unique-indices es #+nil(remove nil es))))

