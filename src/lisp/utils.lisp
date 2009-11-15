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


;;; --- Money --------------------

(defun cents (euros)
  (ftruncate (+ (* (signum euros) 0.5)
		(* 100 euros))))

(defun euro (cents)
  (/ cents 100))



