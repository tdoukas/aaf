(in-package :aaf)

(defclass rc ()
  ((num-threads :accessor rc-num-threads :initarg :num-threads :initform 1)
   (start :accessor rc-start :initarg :start)
   (end :accessor rc-end :initarg :end)
   (runningp :accessor rc-runningp :initform t)
   (result :accessor rc-result :initform ())
   (timestamp :accessor rc-timestamp :initform 0)))

(defgeneric run-rc (rc))
(defgeneric rc-body (rc chunk))

(defun rc-periodically (rc fn &key (seconds 5))
  (let ((time (get-universal-time)))
    (when (>= (- time (rc-timestamp rc)) seconds)
      (funcall fn)
      (setf (rc-timestamp rc) time))))

(defclass rc-chunk ()
  ((start :accessor rc-chunk-start :initarg :start)
   (end :accessor rc-chunk-end :initarg :end)
   (thread :accessor rc-chunk-thread :initarg :thread)
   (t0 :accessor rc-chunk-t0 :initform (get-universal-time))
   (timestamp :accessor rc-chunk-timestamp :initform 0)))

(defmethod print-object ((chunk rc-chunk) out)
  (print-unreadable-object (chunk out :type t)
    (format out "~a: ~a .. ~a" (rc-chunk-thread chunk) (rc-chunk-start chunk) (rc-chunk-end chunk))))

(defun rc-chunk-periodically (chunk fn &key (seconds 5))
  (let ((time (get-universal-time)))
    (when (>= (- time (rc-chunk-timestamp chunk)) seconds)
      (funcall fn)
      (setf (rc-chunk-timestamp chunk) time))))

(defmethod run-rc ((rc rc))
  (let ((package *package*)
	(chunks (mapcar (lambda (range)
			  (destructuring-bind (thread start end) range
			    (make-instance 'rc-chunk :thread thread :start start :end end)))
			(%partition-range (rc-start rc) (rc-end rc) (rc-num-threads rc)))))
    (unwind-protect
	 (let ((threads (loop for i below (rc-num-threads rc) for chunk in chunks
			      collect
			      (let ((chunk chunk))
				(sb-thread:make-thread
				 (lambda (&aux (*package* package))
				   (rc-body rc chunk)))))))
	   (dolist (thread threads)
	     (sb-thread:join-thread thread)))
      (setf (rc-runningp rc) nil))
    (rc-result rc)))

(defun %partition-range (start end num)
  (let* ((diff (- end start))
	 (size (/ diff num))
	 chunks
	 (p start))
    (dotimes (i num)
      (let ((s p)
	    (e (min end (round (+ p size)))))
	(push (list i s e) chunks)
	(setq p (min end (1+ e)))))
    (nreverse chunks)))

(defgeneric rc-loop-body (rc chunk i))

(defmethod rc-body ((rc rc) (chunk rc-chunk))
  (logmsg "Start thread ~s" chunk)
  (loop for i from (rc-chunk-start chunk) upto (rc-chunk-end chunk) while (rc-runningp rc) do   
    (rc-loop-body rc chunk i)
    (rc-chunk-periodically chunk (lambda () (%log-progress chunk i)))))
	
(defun %log-progress (chunk bitmap)
  (let* ((total (- (rc-chunk-end chunk) (rc-chunk-start chunk)))
	 (current (- bitmap (rc-chunk-start chunk)))
	 (ratio (/ current total))
	 (time (- (get-universal-time) (rc-chunk-t0 chunk)))
	 (seconds (round (or (ignore-errors (* (- 1 ratio) (/ time ratio))) -1))))
    (logmsg "~a (~5$% ~a): ~a" (rc-chunk-thread chunk) (* 100 ratio) (seconds-to-time seconds) bitmap)))

;;;
;;; Utils
;;;

(defparameter *mutex* (sb-thread:make-mutex))

(defun logmsg (format-string &rest args)
  (sb-thread:with-mutex (*mutex*)
    (apply #'format t format-string args)
    (fresh-line)))

(defun seconds-to-time (seconds)
  (if (< seconds 0) "??:??:??" 
      (let* ((hours (truncate seconds 3600))
	     (seconds (- seconds (* hours 3600)))
	     (minutes (truncate seconds 60))
	     (seconds (- seconds (* minutes 60))))
	(format nil "~2,'0d:~2,'0d:~2,'0d" hours minutes seconds))))
