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
  (navbar `(#|(general      ,(config)                           "Γενικά")|#
            (city         ,(city)                             "Πόλεις")
            (bank         ,(bank)                             "Τράπεζες")
            (tof          ,(tof)                              "Δ.Ο.Υ.")
            (account      ,(account)                          "Λογαριασμοί")
            #|#|(cheque-stran ,(config/cheque-stran "receivable") "Επιταγές")|#|#)
          :css-class "section-navbar hnavbar grid_12"
          :active active))

(defpage dynamic-page config ("config/") ()
  (with-document ()
    (:head
     (:title "Ρυθμίσεις")
     (config-headers))
    (:body
     (:div :id "container" :class "container_12"
           (header 'config)
           (config-navbar 'general)
           (:div :id "bank-window" :class "window grid_10"
                 (:div :class "title" "Ρυθμίσεις » Γενικά")
                 "nothing...")))))