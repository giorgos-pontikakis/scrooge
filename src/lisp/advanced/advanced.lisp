(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun advanced-navbar (&optional active)
  (with-html
    (:div :class "grid_12"
      (:div :class "section-navbar"
        (navbar `((account ,(account)
                           "Λογαριασμοί")
                  (tx      ,(tx)
                           "Καθολικό"))
                :css-class "hnavbar"
                :active active)))))
