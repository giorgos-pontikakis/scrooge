(in-package :scrooge)

(defun string-upcase-gr (string)
  (let ((result-string (string-upcase string)))
    (mapc #'(lambda (pair)
	      (nsubstitute (cdr pair)
			   (car pair)
			   result-string))
	  '((#\Ά . #\A)
	    (#\Έ . #\Ε)
	    (#\Ή . #\Η)
	    (#\Ί . #\Ι)
	    (#\Ό . #\Ο)
	    (#\ς . #\Σ)
	    (#\Ύ . #\Υ)
	    (#\Ώ . #\Ω)
	    (#\ΐ . #\Ϊ)
	    (#\ΰ . #\Ϋ)))
    result-string))


(defun merge-nonnull (initial new)
  (if (null new)
      initial
      (mapcar (lambda (i n)
		(or n i))
	     initial new)))

(defparameter *styles*
  '((attention . "attention")))

(defun style (symbol)
  (cdr (assoc symbol *styles*)))

;; (defun style-invalid-fn ()
;;   (lambda (param)
;;     (if (validp param) nil (style 'attention))))

(defun style-invalid-p (param)
  (if (or (null param) (validp param)) nil (style 'attention)))


;; (defun merge-values (values db-values)
;;   (iter (for key in db-values by #'cddr) 
;;         (for db-val in (rest db-values) by #'cddr)
;;         (collect key)
;;         (let ((val (getf values key)))
;;           (if val (collect val) (collect db-val)))))




