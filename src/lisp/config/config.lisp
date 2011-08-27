(in-package :scrooge)



;;; config crud rows

(defclass config-row (crud-row/dao)
  ())

(defmethod payload ((row config-row) enabled-p)
  (make-instance 'textbox
                 :name 'title
                 :value (title (record row))
                 :disabled (not enabled-p)))



;;; UI elements

(defun config-navbar (&optional active-page-name)
  (navbar `((general      ,(config)                           "Γενικά")
            (city         ,(city)                             "Πόλεις")
            (bank         ,(bank)                             "Τράπεζες")
            (tof          ,(tof)                              "Δ.Ο.Υ.")
            (account      ,(account)                          "Λογαριασμοί")
            (cheque-stran ,(config/cheque-stran "receivable") "Επιταγές"))
          :css-class "section-navbar hnavbar grid_12"
          :active-page-name active-page-name))



;;; Config utilities

(defun config-data (table-name filter)
  (with-db ()
    (select-dao table-name (ilike filter))))
