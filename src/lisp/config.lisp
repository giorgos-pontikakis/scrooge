(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

(define-navbar config-navbar () (:id "subnavbar" :ul-style "hmenu")
  (banks    (banks)    "Τράπεζες")
  (tofs     (tofs)     "Δ.Ο.Υ.")
  (accounts (accounts) "Λογαριασμοί")
  (temtx    (temtx)    "Πρότυπες Συναλλαγές")
  (stran    (stran)      "Καταστατικές Μεταβολές"))


;;; Config main page

(define-dynamic-page config () ("config/")
  (no-cache)
  (with-page ()
    (:head
     (:title "Ρυθμίσεις")
     (css-standard-headers))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'config)
	   (config-navbar nil))
     (:div :id "body" 
	   (:div :id "content" :class "window"
		 "Don't touch")
	   (footer)))))



(defun make-sql-config (table-name)
  (sql-compile
   `(:select 'id 'title :from ,table-name)))