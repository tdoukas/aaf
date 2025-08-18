(in-package :aaf)

;;
;;  Generation of Example AFs
;;

(defun generate-aaf (n &key (id -1) (ar 1.0) num-attacks exclude-attacks)
  (let* ((chars '(a b c d e f g h i j k l m n o p q r s t u v w))
	 (args (if (> n (length chars))
		   (loop for i from 1 upto n collect (intern (format nil "a~a" i)))
		   (subseq chars 0 n)))
	 (all-attacks (%exclude (%attacks-for-id args id) exclude-attacks))
	 (attacks (if num-attacks (%keep-n all-attacks num-attacks)
		      (%discard all-attacks (- 1.0 ar)))))
    (make-aaf args attacks)))

(defun %attacks-for-id (args id)
  (let ((n (length args))
	attacks)
    (dotimes (i n)
      (dotimes (j n)
	(when (logbitp (+ j (* i n)) id)
	  (push (list (nth i args) (nth j args)) attacks))))
    (nreverse attacks)))

(defun %discard (xs r)
  (let ((n (round (* r (length xs)))))
    (dotimes (%j n)
      (let ((i (random (length xs))))
	(setf xs (loop for x in xs for n from 0 unless (= i n) collect x)))))
  xs)

(defun %keep-n (xs n &aux (l (length xs)))
  (if (> l n)
      (%keep-n (loop with m = (random l) for x in xs for i from 0 unless (= i m) collect x) n)
      xs))

(defun %exclude (xs es)
  (loop for x in xs when (every (lambda (e) (not (subsetp x e))) es) collect x))

;;
;;  Generation of Example Extensions
;;

(defun generate-extension-set (n m)
  "Generate M extensions, each randomly selected from 2^n possible subsets."
  (remove-duplicates (loop for i from 0 below m collect (generate-subset n)) :test #'set-equal))

(defun generate-subset (n &key (id (random (expt 2 n))))
  (let ((args (subseq '(a b c d e f g h i j k l m n o p q r s t u v w) 0 n)))
    (loop for i from 0 below n when (logbitp i id) collect (nth i args))))

(defun generate-subset-from-base-set (set &key (id (random (expt 2 (length set)))))
  (let ((n (length set)))
    (loop for i from 0 below n when (logbitp i id) collect (nth i set))))
