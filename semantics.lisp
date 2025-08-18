(in-package :aaf)

;;
;;  Extensions of AAFs
;;

(defun enumerate-conflict-free-sets (f function)
  "Call FUNCTION with each conflict-free set of arguments of F."
  (enumerate-subsets (aaf-arguments f)
		     (lambda (s)
		       (when (conflict-free s (aaf-attacks f))
			 (funcall function s)))))

(defun collect-conflict-free-sets (f &optional (filter (constantly t)))
  "Collect the conflict free sets of F that match FILTER."
  (let (es)
    (enumerate-conflict-free-sets
     f (lambda (s) (when (funcall filter s)
		     (push s es))))
    (nreverse es)))

(defun any-conflict-free-set (aaf &optional (pred (constantly t)))
  "Return one conflict-free set to which PRED applies."
  (block fn
    (enumerate-conflict-free-sets
     aaf (lambda (s) (when (funcall pred s)
		       (return-from fn s))))))

(defun cf-extensions (f)
  "Return the conflict-free extensions of F."
  (collect-conflict-free-sets f))

(defun admissible-extensions (f)
  "Return the admissible extensions of F."
  (collect-conflict-free-sets
   f (lambda (s) (defends-itself s (aaf-attacks f)))))

(defun stable-extensions (f)
  "Return the stable extensions of F."
  (flet ((attacks-complement (s)
	   (let ((c (remove-if (lambda (a) (member a s)) (aaf-arguments f))))
	     (every (lambda (a) (set-attacks f s a)) c))))
    (collect-conflict-free-sets f #'attacks-complement)))

(defun preferred-extensions (f)
  "Return the preferred extensions of F."
  (maximal-sets (admissible-extensions f)))

(defun complete-extensions (f)
  "Return the complete extensions of F."
  (remove-if-not (lambda (e)
		   (every (lambda (arg)
			    (or (not (defends-arg e arg (aaf-attacks f)))
				(member arg e)))
			  (aaf-arguments f)))
		 (admissible-extensions f)))

(defun naive-extensions (f)
  (maximal-sets (cf-extensions f)))

(defun semi-stable-extensions (f)
  (let ((comp (complete-extensions f)))
    (remove-if-not
     (lambda (s)
       (let ((s-range (range f s)))
	 (every (lambda (s1)
		  (subsetp (range f s1) s-range))
		comp)))
     comp)))

(defun stage-extensions (f)
  (let ((cf (cf-extensions f)))
    (remove-if-not
     (lambda (s)
       (let ((s-range (range f s)))
	 (every (lambda (s1)
		  (subsetp (range f s1) s-range))
		cf)))
     cf)))

(defun prudent-admissible-extensions (f &aux (atts (aaf-attacks f)) sets)
  (enumerate-subsets (aaf-arguments f)
		     (lambda (s)
		       (when (and (defends-itself s atts)
				  (indirect-conflict-free s atts))
			 (push s sets))))
  sets)

(defun prudent-preferred-extensions (f)
  (maximal-sets (prudent-admissible-extensions f)))
  
;;
;;  Weakly Admissible
;;

(defun reduct (aaf e)
  (let ((e* (remove-if (lambda (a)
			 (or (member a e)
			     (set-attacks aaf e a)))
		       (aaf-arguments aaf))))
    (make-aaf e* (remove-if-not (lambda (p) (subsetp p e*))
				(aaf-attacks aaf)))))

(defun weakly-admissible-p (f e)
  (and (conflict-free e (aaf-attacks f))
       (let ((attackers (remove-if-not (lambda (a)
					 (attacks-set f a e))
				       (aaf-arguments f)))
	     (r (reduct f e)))
	 (every (lambda (y)
		  (block blk
		    (enumerate-subsets
		     (aaf-arguments r)
		     (lambda (s)
		       (when (and (member y s)
				  (weakly-admissible-p r s))
			 (return-from blk nil))))
		    t))
		attackers))))

(defun weakly-admissible-extensions (f)
  "Return the weakly admissible extensions of F."
  (collect-conflict-free-sets f (lambda (s) (weakly-admissible-p f s))))

(defun weakly-preferred-extensions (f)
  "Return the weakly preferred extensions of F."
  (maximal-sets (weakly-admissible-extensions f)))

;;
;;  Weak Defense
;;

(defun weakly-defends-p (f e x)
  "Tell whether E weakly defends X in F."
  (let ((attackers (remove-if-not (lambda (a) (attacks-set f a x)) (aaf-arguments f)))
	(wadm-f (weakly-admissible-extensions f))
	(union-wadm-fe (union* (weakly-admissible-extensions (reduct f e)))))
    (every (lambda (y)
	     (or (set-attacks f e y)
		 (and (not (member y e))
		      (not (member y union-wadm-fe))
		      (some (lambda (xp) (subsetp x xp)) wadm-f))))
	   attackers)))

;;
;;  Undisputed
;;

(defun vacuousp (f semantics)
  (let ((extensions (funcall semantics f)))
    (if (subsetp extensions (list nil) :test #'subsetp) t
	(values nil extensions))))

(defun adm-vacuousp (aaf)
  (let ((e (any-conflict-free-set aaf (lambda (s) (and s (defends-itself s (aaf-attacks aaf)))))))
    (if e (values nil e) t)))

(defun vacuous-reduct-extensions (f sigma tau)
  (remove-if-not (lambda (e) (vacuousp (reduct f e) tau)) (funcall sigma f)))

(defun undisputed-extensions (f)
  (vacuous-reduct-extensions f #'cf-extensions #'admissible-extensions))

(defun strongly-undisputed-extensions (f)
  (vacuous-reduct-extensions f #'cf-extensions #'undisputed-extensions))

;;
;;  Weakly Admissible Labelling
;;

(defun cross-product (as bs)
  (let (mappings (l (length bs)))
    (dotimes (i (expt l (length as)))
      (let ((x i) mapping)
	(dolist (arg as)
	  (push (list arg (nth (mod x l) bs)) mapping)
	  (setq x (truncate x l)))
	(push (nreverse mapping) mappings)))
    (nreverse mappings)))

(defun verify-weakly-admissible-mapping (f ms)
  (labels ((label (a)
	     (second (find a ms :key #'first)))
	   (inp (a)
	     (eql :in (label a)))
	   (undecp (a)
	     (eql :undec (label a))))
    (every (lambda (m)
	     (destructuring-bind (arg label) m
	       (let ((attackers (attackers-of f (list arg))))
		 (case label
		   (:in (notany #'inp attackers))
		   (:out (some #'inp attackers))
		   (:undec (and (notany #'inp attackers)
				(some #'undecp attackers)))))))
	   ms)))

(defun weakly-admissible-labellings (f)
  (remove-if-not (lambda (m) (verify-weakly-admissible-mapping f m))
		 (cross-product (aaf-arguments f) '(:in :out :undec))))
