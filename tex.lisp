(in-package :aaf)

(defun node-label (name)
  (format nil "$~a$" (simple-node-label name)))

(defun simple-node-label (name)
  (let ((s (string-downcase name)))
    (if (> (length s) 1) (format nil "~a_{~a}" (subseq s 0 1) (subseq s 1)) (format nil "~a" s))))  

(defun print-embedding (e out &key (scale 1))
  (format out "\\begin{tikzpicture}[scale=~a,every node/.style={scale=~a},rotate=0]~%" scale scale)
  (dolist (node (embedding-nodes e))
    (format out "  \\node[draw, circle, minimum size=0.8cm] (~a) at (~a, ~a) {~a};~%"
	    (node-name node) (node-x node) (node-y node) (node-label (node-name node))))
  (dolist (arrow (embedding-arrows e))
    (let ((from (arrow-from arrow))
	  (to (arrow-to arrow)))
      (if (eql to from) (format out "  \\path[->,draw] (~a) to [out=100,in=160,looseness=4.8] (~a);~%"
				(node-name from) (node-name to))
	  (format out "  \\path[draw, ->] (~a) -- (~a);~%"
		  (node-name from) (node-name to)))))
  (format out "\\end{tikzpicture}~%"))

(defun output-embedding (aaf &key (filename "fig.tex") (scale 1) caption)
  (let ((fr (make-instance 'fr))
	(e (embed aaf)))
    (loop while (not (embedder-finished-p fr e)) do
      (embed-step fr e))
    (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
      (print-embedding e out :scale scale)
      (when caption
	(format out "\\caption{~a}~%" caption))))
  aaf)

#-linux
(defun run-latex (&optional (texfile "figure.tex"))
  (sb-ext:run-program
   "pdflatex.exe" (list texfile)))

#+linux
(defun run-latex (&optional (texfile "figure.tex"))
  (sb-ext:run-program "/usr/bin/pdflatex" (list texfile)))

(defvar *default-semantics* 'admissible-extensions)

(defun output-example (aaf &key n (semantics (list *default-semantics*)))
  (output-embedding
   aaf :caption (format nil "~{~a~}"
			(mapcar (lambda (s)
				  (format nil "~a=\\\\\\hspace{\\textwidth}$~a$\\\\\\hspace{\\textwidth}"
					  s (print-extension-set (sort-set (funcall s aaf)) nil)))
				semantics))
       :filename (if n (format nil "fig~a.tex" n) "fig.tex"))
  (run-latex (if n "figures.tex" "figure.tex"))
  aaf)

(defun print-extension-set (es &optional (out t))
  (format out "\\{~{~a~^,~}\\}"
	  (mapcar (lambda (e)
		    (if e (format nil "\\{~{~a~^,~}\\}" (mapcar #'simple-node-label e))
			"\\emptyset"))
		  es)))
		    
(defun print-set (s &optional (out t))
  (if s (format out "\\{~{~a~^,~}\\}" s) (princ "\\empyset" out)))
