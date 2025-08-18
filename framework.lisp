(in-package :aaf)

;;
;;  AAF
;;

(defclass aaf ()
  ((arguments :reader aaf-arguments :initarg :arguments)
   (attacks :reader aaf-attacks :initarg :attacks)))
   
(defun make-aaf (arguments attacks &key (create-missing t))
  "Create an AAF from given ARGUMENTS and ATTACKS."
  (when create-missing
    (mapcar (lambda (a) (pushnew a arguments)) (union* attacks)))
  (make-instance 'aaf :arguments (sort-set arguments) :attacks attacks))

(defmethod print-object ((aaf aaf) out)
  (print-unreadable-object (aaf out :type t)
    (format out ":arguments ~s :attacks ~s"
	    (aaf-arguments aaf)
	    (aaf-attacks aaf))))

(defun aaf-equal (aaf1 aaf2)
  (and (set-equal (aaf-arguments aaf1) (aaf-arguments aaf2))
       (set-equal (aaf-attacks aaf1) (aaf-attacks aaf2))))

;;
;;  Basic operations on AAFs
;;

(defun aaf-empty-p (aaf)
  (null (aaf-arguments aaf)))

(defun attacks (f a b)
  "For a given F, tell whether A attacks B."
  (member (list a b) (aaf-attacks f) :test #'equal))

(defun set-attacks (f s a)
  "For a given F, tell whether S set-attacks A."
  (some (lambda (b) (attacks f b a)) s))

(defun attacks-set (f a s)
  "For a given F, tell whether A attacks some argument in S."
  (some (lambda (b) (attacks f a b)) s))

(defun set-attacks-set (f s1 s2)
  "For a given F, tell whether S1 attacks S2."
  (some (lambda (a) (attacks-set f a s2)) s1))

(defun attacked-from (aaf x)
  "For AAF, compute X^+."
  (remove-duplicates
   (mapcar #'second (remove-if-not (lambda (att) (member (first att) x)) (aaf-attacks aaf)))))

(defun attackers-of (aaf x)
  "For AAF, compute X^-."
  (remove-duplicates
   (mapcar #'first (remove-if-not (lambda (att) (member (second att) x)) (aaf-attacks aaf)))))

(defun range (aaf s)
  "Compute the range of S w.r.t. AAF."
  (union (aaf-arguments aaf) (attacked-from aaf s)))

(defun aaf-restriction (aaf args)
  (make-aaf args (remove-if-not (lambda (attack)
				  (and (member (first attack) args)
				       (member (second attack) args)))
				(aaf-attacks aaf))))

(defun remove-self-attackers (aaf)
  (let ((self-attackers (remove-if-not (lambda (a) (member (list a a) (aaf-attacks aaf) :test #'set-equal))
				       (aaf-arguments aaf))))
    (aaf-restriction aaf (set-difference (aaf-arguments aaf) self-attackers))))

(defun aaf-covers-p (aaf1 aaf2)
  "Tell whether AAF1 covers AAF2."
  (and (subsetp (aaf-arguments aaf2) (aaf-arguments aaf1))
       (subsetp (aaf-attacks aaf2) (aaf-attacks aaf1) :test #'equal)))

(defun rename-node (aaf old new)
  (let ((new-args (mapcar (lambda (a) (if (eql a old) new a)) (aaf-arguments aaf)))
	(new-atts (mapcar (lambda (att)
			    (destructuring-bind (from to) att
			      (list (if (eql from old) new from)
				    (if (eql to old) new to))))
			  (aaf-attacks aaf))))
    (make-aaf new-args new-atts)))

(defun rename-nodes (aaf list)
  (mapcar (lambda (i)
	    (destructuring-bind (old new) i
	      (setq aaf (rename-node aaf old new))))
	  list)
  aaf)

(defun augment-framework (aaf args attacks)
  (make-aaf (append args (copy-list (aaf-arguments aaf)))
	    (append attacks (copy-list (aaf-attacks aaf)))))

(defun merge-args (aaf args b)
  (labels ((replc (a)
	     (if (member a args) b a)))
    (make-aaf (remove-duplicates (mapcar #'replc (aaf-arguments aaf)))
	      (remove-duplicates (mapcar (lambda (att) (mapcar #'replc att)) (aaf-attacks aaf))
				 :test #'equal))))

;;
;;  Print a form that constructs the framework
;;

(defun aaf-form (aaf &aux (*print-pretty* nil))
  (values (read-from-string (format nil "(make-aaf '~s '~s)" (aaf-arguments aaf) (aaf-attacks aaf)))))

;;
;;  Syntactic Features
;;

(defun cycle-p (aaf args)
  "Tell whether ARGS lie on a cycle in AAF, and return a proof."
  (block fun
    (let ((links (mapcar #'list args)) links2)
      (dotimes (i (1- (length args)))
	(dolist (link links)
	  (dolist (link2 links)
	    (when (and (not (eql link link2))
		       (attacks aaf (car (last link)) (first link2)))
	      (push (append link link2) links2))))
	(setq links links2 links2 nil))
      (dolist (list links)
	(let ((subl (subseq list 0 (1+ (length args)))))
	  (when (and (subsetp args subl)
		     (eql (first subl) (car (last subl))))
	    (return-from fun subl)))))))

(defun all-cycles (aaf arg)
  "Return all cycles that ARG lies on in AAF."
  (let ((chains (list (list arg))) chains2 results)
    (loop  while chains do
      (dolist (chain chains)
	(let ((last (car (last chain))))
	  (dolist (att (aaf-attacks aaf))
	    (when (eql last (car att))
	      (push (append chain (list (second att))) chains2)))))
      (setq chains nil)
      (dolist (chain chains2)
	(if (= 2 (count arg chain))
	    (push chain results)
	    (when (eql (length (remove-duplicates chain)) (length chain))
	      (push chain chains))))
      (setq chains2 nil))
    results))
