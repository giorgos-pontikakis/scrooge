(in-package :scrooge)

(defmethod bool ((boolean symbol))
  (if (null boolean) "f" "t"))

(defmethod bool ((boolean string))
  (string= "t" boolean))

(defun cents (euros)
  (ftruncate (+ (* (signum euros) 0.5)
		(* 100 euros))))

(defun euro (cents)
  (/ cents 100))