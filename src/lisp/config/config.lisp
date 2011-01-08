(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; UI elements

(defun config-menu (&optional active-page-name)
  (display (make-instance 'horizontal-navbar
                          :id "config-menu"
                          :style "hnavbar grid_12"
                          :spec '((city "Πόλεις")
                                  (bank "Τράπεζες")
                                  (tof "Δ.Ο.Υ.")
                                  (account "Λογαριασμοί")
                                  (temtx "Πρότυπες Συναλλαγές")))
           :active-page-name active-page-name))


(defun main-menu (&optional active-page-name)
  (display (make-instance 'horizontal-navbar
                          :id "config-menu"
                          :style "hnavbar grid_12"
                          :spec '((company "Εταιρίες")))
           :active-page-name active-page-name))


;;; Config main page

(define-dynamic-page config ("config/") ()
  (no-cache)
  (with-document ()
    (:head
     (:title "Ρυθμίσεις")
     (global-headers))
    (:body
     (:div :id "container" :class "container_12"
           (header 'config)
           (config-menu)
           (:div :id "body"
                 (:p "Config content not yet available"))))))

(define-dynamic-page main ("main/") ()
  (no-cache)
  (with-document ()
    (:head
     (:title "Οικονομικά")
     (global-headers))
    (:body
     (:div :id "container" :class "container_12"
           (header 'main)
           (main-menu nil)
           (:div :id "body"
                 (:p "Main content not yet available"))))))



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
