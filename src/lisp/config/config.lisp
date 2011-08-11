(in-package :scrooge)



;;; config crud rows

(defclass config-row (scrooge-crud-row)
  ())

(defmethod payload ((row config-row) enabled-p)
  (make-instance 'textbox
                 :name 'title
                 :value (getf (record row) :title)
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
    (query (sql-compile
            (if filter
                `(:order-by (:select 'id 'title :from ,table-name
                                     :where (:ilike 'title ,(ilike filter)))
                            'title)
                `(:order-by (:select 'id 'title :from ,table-name)
                            'title)))
           :plists)))

(defclass page-family-mixin ()
  ((filter-parameter-names :accessor filter-parameter-names :initarg :filter-parameter-names)
   (system-parameter-names :accessor system-parameter-names :initarg :system-parameter-names)))
