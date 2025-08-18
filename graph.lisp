(in-package :aaf)

(defun neighbours (a edges)
  "Return the neighbours of A as stored in EDGES."
  (mapcar #'second (remove-if-not (lambda (edge) (eql a (first edge))) edges)))

(defun all-paths (a b edges &aux paths)
  "In a digraph given by EDGES, return all paths between A and B."
  (labels ((try-reach (revpath b edges)
	     (let ((a (first revpath)))
	       (if (eql a b) (push revpath paths)
		   (dolist (n (remove a (neighbours a edges)))
		     (try-reach (list* n revpath) b
				(remove-if (lambda (edge) (eql a (first edge))) edges)))))))
    (try-reach (list a) b edges)
    paths))

