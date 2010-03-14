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


;; (defun get-val-or-default (param-name params defaults)
;;   (or (val* (find param-name params :key #'name))
;;       (getf defaults param-name)))

;; (defun get-error-style (param-name params)
;;   (let ((p (find param-name params :key #'name)))
;;     (if (or (null p) (validp p)) nil "attention")))

;; (defun merged-val (param defaults)
;;   (or (val param)
;;       (raw param)
;;       (find (name param) defaults :test #'string-equal)))

;; (defun make-plist (keys values)
;;   (iter (for k in keys)
;; 	(for v in values)
;; 	(collect k)
;; 	(collect v)))


;; (defun val+ (name params)
;;   (let ((param (find name params :key #'name)))
;;     (if param (val param) nil)))

;; (defun raw+ (name params)
;;   (raw (find name params :key #'name)))