(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; UI elements

(defun config-menu (active-page-name)
  (display (make-instance 'horizontal-menu
                          :id "config-menu"
                          :spec '((cities "Πόλεις")
                                  (banks "Τράπεζες")
                                  (tofs "Δ.Ο.Υ.")
                                  #|(accounts "Λογαριασμοί")|#
                                  #|(temtx "Πρότυπες Συναλλαγές")|#
                                  #|(stran "Καταστατικές Μεταβολές")|#))
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
           (:div :class "clear")
           (:div :id "body"
                 (:p "Config content not yet available"))))))



;;; Config utilities

(defun config-data (table-name)
  (with-db ()
    (query (sql-compile
            `(:select 'id 'title :from ,table-name))
           :plists)))
