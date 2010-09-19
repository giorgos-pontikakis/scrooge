(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; Widgets

(defun config-navbar (active)
  (render (hnavbar)
          :id "subnavbar"
          :page-specs '((bank "Τράπεζες")
                        (tof  "Δ.Ο.Υ.")
                        (city "Πόλεις")
                        ;; (accounts "Λογαριασμοί")
                        ;; (temtx	   "Πρότυπες Συναλλαγές")
                        ;; (stran	   "Καταστατικές Μεταβολές")
                        )
          :active-page-name active))



;;; Config main page

(define-dynamic-page config () ("config/")
  (no-cache)
  (with-document ()
    (:head
     (:title "Ρυθμίσεις")
     (head-css-std))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'config)
	   (config-navbar nil))
     (:div :id "body" 
	   (:div :id "content" :class "window"
		 "Don't touch")
	   (footer)))))



;;; Configuration pages 

(defun config-data (table-name)
  (with-db ()
    (query (sql-compile
            `(:select 'id 'title :from ,table-name))
           :plists)))

(defun config-header (config-choice)
  (with-html
    (:div :id "header"
          (logo)
          (primary-navbar 'config)
          (config-navbar config-choice))))


