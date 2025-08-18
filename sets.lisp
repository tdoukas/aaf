(in-package :aaf)

;;
;;  Basic Set Operations
;;

(defun proper-subset-p (subs supers &key (test #'equal))
  "Tell whether SUBS is a proper subset of SUPERS."
  (and (not (equal subs supers))
       (subsetp subs supers :test test)))

(defun set-equal (s1 s2 &key (test #'equal))
  "Tell whether S1 and S2 contain the same elements."
  (and (subsetp s1 s2 :test test)
       (subsetp s2 s1 :test test)))

(defun es-equal (s1 s2)
  (set-equal s1 s2 :test #'set-equal))

(defun power-set (s)
  "Return the set of all subsets of S."
  (loop for bitmap from 0 below (expt 2 (length s))
	collect
	(loop for i from 0 below (length s)
	      when (logbitp i bitmap) collect (nth i s))))

(defun downward-closure (ss)
  "Return the set of all subsets if the individual elements of SS."
  (remove-duplicates (mapcan #'power-set ss) :test #'set-equal))

(defun union* (ss)
  "Return the union of the sets of SS."
  (if (null ss) nil (reduce #'union ss)))

(defun maximal-sets (ss)
  "Return the maximal (with regard to inclusion) sets of SS."
  (remove-if (lambda (s) (find s ss :test #'proper-subset-p)) ss))

(defun have-set (s ss)
  "Tell whether set S is present in a set of sets SS."
  (member s ss :test #'set-equal))

;;
;;  Set Utilities
;;

(defun all-pairs (es)
  "List all pairs of different elements of ES."
  (let (pairs)
    (loop with es* = es while es* for e1 = (pop es*) do
      (loop for e2 in es* do
	(push (list e1 e2) pairs)))
    (nreverse pairs)))

(defun all-pairs-symmetrical (as)
  (let (pairs)
    (dolist (a1 as)
      (dolist (a2 as)
	(push (list a1 a2) pairs)))
    pairs))

(defun sort-set (e)
  "Sort the set lexicographically."
  (let ((res (sort (copy-list e) #'string-lessp :key #'prin1-to-string)))
    (when (and (> (length res) 1) (listp (first res)))
      (setq res (sort (copy-list res) #'< :key #'length)))
    res))

(defun sort-extension-set (es)
  (sort-set (mapcar #'sort-set es)))

(defun cluster-elements (set &key (test #'equal) (union #'union))
  "When any two elements of SET satisfy TEST, combine them using UNION. Repeat."
  (loop while (block loops
		(loop for i from 0 below (length set) do
		  (loop for j from 1 below (length set) do
		    (let ((e1 (nth i set))
			  (e2 (nth j set)))
		      (when (funcall test e1 e2)
			(setq set (remove e1 set)
			      set (remove e2 set))
			(push (funcall union e1 e2) set)
			(return-from loops t)))))))
  set)
      
(defun nth-permutation (list n)
  (when list
    (let ((element (nth (mod n (length list)) list)))
      (cons (nth (mod n (length list)) list)
	    (nth-permutation (remove element list) (floor n (length list)))))))

(defun fact (n)
  (assert (>= n 0))
  (if (zerop n) 1 (* n (fact (1- n)))))

;;
;;  Subsets and Attacks
;;

(defun enumerate-subsets (as function)
  "Call FUNCTION with each subset of AS."
  (let* ((n (length as))
	 (m (expt 2 n)))
    (dotimes (i m)
      (funcall function
	       (loop for b from 0 below n
		     when (not (zerop (logand (expt 2 b) i)))
		       collect (nth b as))))))

(defun similar-pairs-p (p1 p2)
  "Tell whether the pairs P1 and P2 contain the same arguments."
  (destructuring-bind (a1 b1) p1
    (destructuring-bind (a2 b2) p2
      (or (and (eql a1 a2)
	       (eql b1 b2))
	  (and (eql a1 b2)
	       (eql a2 b1))))))

(defun conflict-free (s attacks)
  "Tell whether S is conflict-free under the given ATTACKS."
  (block fn
    (dolist (a s)
      (dolist (b s)
	(when (find (list a b) attacks :test #'similar-pairs-p)
	  (return-from fn nil))))
    t))

(defun indirect-conflict-free (s attacks)
  "Tell whether S has indirect conflicts under given ATTACKS."
  (block fn
    (dolist (a s)
      (dolist (b s)
	(unless (eql a b)
	  (when (some (lambda (path) (evenp (length path))) (all-paths a b attacks))
	    (return-from fn nil)))))
    t))

(defun defends-itself (s attacks)
  "Tell whether S defends itself under the given ATTACKS."
  (let ((s- (mapcan (lambda (p)
		      (when (member (second p) s)
			(list (first p))))
		    attacks)))
    (every (lambda (a)
	     (some (lambda (p)
		     (and (eql (second p) a)
			  (member (first p) s)))
		   attacks))
	   s-)))

(defun defends-arg (s a attacks)
  "Tell whether S defends argument A under given ATTACKS."
  (let ((attackers (mapcar #'first (remove-if-not (lambda (att)
						    (eql a (second att)))
						  attacks))))
    (every (lambda (attacker)
	     (some (lambda (att)
		     (and (eql (second att) attacker)
			  (member (first att) s)))
		   attacks))
	   attackers)))

;;;
;;;  ES-Isomorphisms
;;;

(defun frequencies (es)
  (sort (mapcar (lambda (a)
		  (list a (count a es :test #'member)))
		(union* es))
	#'< :key #'second))

(defun %args-in-extensions-upto-length (es len)
  (length (remove-duplicates (apply #'append (remove-if-not (lambda (e) (<= (length e) len)) es)))))

(defun isomorphic-extensions (es1 es2)
  "Tell whether ES1 and ES2 are isomoprhic extensions (heuristic)."
  (and (= (length es1) (length es2))
       (equal (sort (mapcar #'length es1) #'<)
	      (sort (mapcar #'length es2) #'<))
       (= (length (union* es1))
	  (length (union* es2)))
       (loop with max = (apply #'max (mapcar #'length (union es1 es2)))
	     for l from 1 upto max always (= (%args-in-extensions-upto-length es1 l)
					     (%args-in-extensions-upto-length es2 l)))
       (equal (mapcar #'second (frequencies es1))
	      (mapcar #'second (frequencies es2)))))
