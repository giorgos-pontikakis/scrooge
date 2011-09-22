(in-package :scrooge)



;;; config crud table and row classes

(defclass config-table (scrooge-table)
  ())

(defclass config-row (scrooge-row/obj)
  ())

(defmethod payload ((row config-row) enabled-p)
  (make-instance 'textbox
                 :name 'title
                 :value (title (record row))
                 :disabled (not enabled-p)))

;;; Config utilities

(defun config-data (table-name search)
  (with-db ()
    (select-dao table-name (:ilike 'title (ilike search)) 'title)))



;;; UI elements

(defun config-navbar (&optional active)
  (navbar `((city          ,(city)                             "Πόλεις")
            (bank          ,(bank)                             "Τράπεζες")
            (tof           ,(tof)                              "Δ.Ο.Υ.")
            (account       ,(account)                          "Λογαριασμοί")
            (account-role  ,(config/account-role)              "Ρόλοι Λογαριασμών")
            (temtx         ,(config/temtx)                     "Συναλλαγές")
            (cheque-stran  ,(config/cheque-stran "receivable") "Επιταγές")
            (project-stran ,(config/project-stran)             "Έργα"))
          :css-class "section-navbar hnavbar grid_12"
          :active active))
