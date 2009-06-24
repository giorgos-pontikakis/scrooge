(in-package :scrooge)

(defmethod bool ((boolean symbol))
  (if (null boolean) "f" "t"))

(defmethod bool ((boolean string))
  (string= "t" boolean))