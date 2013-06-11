(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defmethod navbar ((section (eql 'advanced)) active)
  (declare (ignore section))
  (with-html
      (:div :class "grid_12"
            (:div :class "section-navbar"
                  (obj 'navbar
                       :spec `((account ,(account) "Λογαριασμοί")
                               (tx ,(tx) "Καθολικό"))
                       :css-class "hnavbar"
                       :active active)))))
