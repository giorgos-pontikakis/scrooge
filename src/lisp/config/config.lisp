(in-package :scrooge)



;;; UI elements

(defun config-navbar (&optional active-page-name)
  (display (make-instance 'horizontal-navbar
                          :style "section-navbar hnavbar grid_12"
                          :spec `((city         ,(city)                "Πόλεις")
                                  (bank         ,(bank)                "Τράπεζες")
                                  (tof          ,(tof)                 "Δ.Ο.Υ.")
                                  (account      ,(account)             "Λογαριασμοί")
                                  (cheque-stran ,(config/cheque-stran "receivable") "Επιταγές")
                                  #|(temtx   ,(temtx)   "Πρότυπες Συναλλαγές")|#))
           :active-page-name active-page-name))



;;; Config main page

(define-dynamic-page config ("config/") ()
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Ρυθμίσεις")
       (global-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-navbar)
             (:div :id "body"
                   (:p "Config content not yet available")))))))



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
