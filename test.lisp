(in-package :aaf)

;;;
;;;  Test Framework
;;;

(defclass test ()
  ())

(defgeneric run-test (test))
(defgeneric generate-test-subject (test))
(defgeneric test-body (test subject))
(defgeneric test-results (test))

(defclass test-result ()
  ())

(defgeneric notablep (test-result))
(defgeneric take-note (test result))
(defgeneric print-result (test-result out))

(defclass repeated-test (test)
  ((num-repeats :reader num-repeats :initarg :num-repeats :initform 1)
   (num-threads :reader  num-threads :initarg :num-threads :initform 1)
   (runningp :accessor runningp :initform t)
   (result :accessor test-result :initform nil)))

(defclass repeated-result (test-result)
  ((notablep :reader notablep :initarg :notablep)
   (properties :reader result-properties :initarg :properties :initform ())))

(defgeneric simplify-result (repeated-test repeated-result))

(defmethod simplify-result ((test repeated-test) obj)
  obj)

(defmethod take-note ((test repeated-test) (result repeated-result))
  (setf (test-result test) result)
  (setf (runningp test) nil))

(defgeneric postprocess-result (test result))

(defmethod test-results ((test repeated-test))
  (let ((res (simplify-result test (test-result test))))
    (when res (postprocess-result test res))))

(defmethod run-test ((test repeated-test))
  (let ((package *package*))
    (unwind-protect
	 (let ((threads (loop for i below (num-threads test)
			      collect (sb-thread:make-thread
				       (lambda (&aux (*package* package))
					 (repeated-test-loop test :thread i))))))
	   (dolist (thread threads)
	     (sb-thread:join-thread thread)))
      (setf (runningp test) nil)))
  (test-results test))

(defvar *log-lock* (sb-thread:make-mutex))

(defmethod repeated-test-loop ((test repeated-test) &key (thread 1))
  (let ((t0 (get-universal-time))
	(t1 0))
    (loop for j below (num-repeats test) while (runningp test) do
      (let* ((subject (generate-test-subject test))
	     (result (test-body test subject)))
	(when (or (notablep result)
		  (let* ((now (get-universal-time))
			 (dt (- now t1)))
		    (when (> dt 1) (setq t1 now))))
	  (sb-thread:with-mutex (*log-lock*)
	    (format t "~s:~s (~s% -- ~s/s) ~a~%" thread j (round (/ (* j 100) (num-repeats test)))
		    (round (/ j (max 1 (- (get-universal-time) t0))))
		    (print-result result nil))))
	(when (notablep result)
	  (take-note test result))))))

(defclass extension-set-result (repeated-result)
  ((es :reader result-es :initarg :es)))

(defmethod print-result ((result extension-set-result) out)
  (format out ":es ~s ~%~1I :properties ~s" (result-es result) (result-properties result)))

(defclass aaf-result (repeated-result)
  ((aaf :reader result-aaf :initarg :aaf)))

(defmethod print-result ((result aaf-result) out)
  (format out ":aaf ~s ~%~1I :properties ~s" (result-aaf result) (result-properties result)))

(defmethod print-object ((result test-result) out)
  (print-unreadable-object (result out :type t)
    (print-result result out)))

(defun simplify-aaf (aaf test)
  (block fn
    (dolist (arg (aaf-arguments aaf))
      (let ((r (aaf-restriction aaf (remove-if (lambda (a) (eql a arg)) (aaf-arguments aaf)))))
	(when (funcall test r)
	  (return-from fn r))))
    (dolist (attack (aaf-attacks aaf))
      (let ((r (make-aaf (aaf-arguments aaf) (remove-if (lambda (att) (eq att attack)) (aaf-attacks aaf)))))
	(when (funcall test r)
	  (return-from fn r))))))

(defmethod simplify-result ((test repeated-test) (result aaf-result))
  (let ((r (simplify-aaf (result-aaf result) (lambda (aaf) (notablep (test-body test aaf))))))
    (cond (r (format t "~s~%" r)
	     (simplify-result test (test-body test r)))
	  (t result))))

(defmethod postprocess-result ((test repeated-test) (result test-result))
  result)

(defun canonicalize-aaf (aaf &key (argmap (mapcar #'list (aaf-arguments aaf)
						  '(a b c d e f g h i j k l m n o p q r s u v w x y))))
  (let* ((args (mapcar #'second argmap))
	 (attacks (mapcar (lambda (att)
			    (flet ((map-arg (a) (second (find a argmap :key #'first))))
			      (list (map-arg (first att)) (map-arg (second att)))))
			  (aaf-attacks aaf))))
    (make-aaf args attacks)))
